From Wasm Require Import datatypes datatypes_properties operations numerics.
From Stdlib Require Import PArith NArith ZArith String List.
From compcert Require cfrontend.Clight cfrontend.Ctypes cfrontend.Cop common.AST common.Errors lib.Integers.
From compcert Require Import export.Ctypesdefs.
From Wasm2c Require Import Ident Util Instantiate Stack Trap.

Import ListNotations.
Import Errors.

Local Open Scope error_monad_scope.

(**
    need to use scratch cells when loading from memory. compcert doesn't allow
    direct read at arbitrary address -- e.g.
                unsigned int v = *(unsigned int * )(mem->data + 3);
    wouldn't be allowed, because the offset isn't a multiple of 4. therefore, we
    use memcpy into scratch cells. need multiple sizes when you're copying
    truncated mem, e.g., 1 byte into an i32.
*)
Definition scratch_u8  : AST.ident := ident_of_scratch 0.
Definition scratch_u16 : AST.ident := ident_of_scratch 1.
Definition scratch_u32 : AST.ident := ident_of_scratch 2.
Definition scratch_u64 : AST.ident := ident_of_scratch 3.
Definition scratch_f32 : AST.ident := ident_of_scratch 4.
Definition scratch_f64 : AST.ident := ident_of_scratch 5.

Definition scratch_vars : list (AST.ident * Ctypes.type) :=
  [(scratch_u8, tuchar); (scratch_u16, tushort); (scratch_u32, tuint);
   (scratch_u64, tulong); (scratch_f32, tfloat); (scratch_f64, tdouble)].

Definition cell_type (cell : AST.ident) : res Ctypes.type :=
  if Pos.eqb cell scratch_u8  then OK tuchar  else
  if Pos.eqb cell scratch_u16 then OK tushort else
  if Pos.eqb cell scratch_u32 then OK tuint   else
  if Pos.eqb cell scratch_u64 then OK tulong  else
  if Pos.eqb cell scratch_f32 then OK tfloat  else
  if Pos.eqb cell scratch_f64 then OK tdouble else
  Error (msg "invalid cell").

(** which scratch cell to use based on load options *)
Definition load_cell (ty : number_type) (trunc : option (packed_type * sx))
  : res AST.ident :=
  match ty, trunc with
  | T_i32, None             => OK scratch_u32
  | T_i64, None             => OK scratch_u64
  | T_f32, None             => OK scratch_f32
  | T_f64, None             => OK scratch_f64
  | T_i32, Some (Tp_i8, _)  => OK scratch_u8
  | T_i32, Some (Tp_i16, _) => OK scratch_u16
  | T_i64, Some (Tp_i8, _)  => OK scratch_u8
  | T_i64, Some (Tp_i16, _) => OK scratch_u16
  | T_i64, Some (Tp_i32, _) => OK scratch_u32
  | _, _ => Error (msg "invalid packed load")
  end.

(** width of load based on load options *)
Definition load_width (ty : number_type) (trunc : option (packed_type * sx))
  : res Z :=
  match ty, trunc with
  | T_i32, None             => OK (4%Z)
  | T_i64, None             => OK (8%Z)
  | T_f32, None             => OK (4%Z)
  | T_f64, None             => OK (8%Z)
  | T_i32, Some (Tp_i8, _)  => OK (1%Z)
  | T_i32, Some (Tp_i16, _) => OK (2%Z)
  | T_i64, Some (Tp_i8, _)  => OK (1%Z)
  | T_i64, Some (Tp_i16, _) => OK (2%Z)
  | T_i64, Some (Tp_i32, _) => OK (4%Z)
  | _, _ => Error (msg "invalid packed load")
  end.

Definition signed_of (t : Ctypes.type) : Ctypes.type :=
  match t with
  | Ctypes.Tint sz _ a => Ctypes.Tint sz Ctypes.Signed a
  | _ => t
  end.

Definition extend_cell (trunc : option (packed_type * sx))
                       (cell : Clight.expr)
                       (cell_ty res_ty : Ctypes.type)
  : Clight.expr :=
  match trunc with
  | Some (_, SX_S) => Clight.Ecast (Clight.Ecast cell (signed_of cell_ty)) res_ty
  | _              => Clight.Ecast cell res_ty (* cell is default unsigned *)
  end.

Definition compile_load (cs : compiler_state)
                        (ty : number_type)
                        (trunc : option (packed_type * sx))
                        (marg : memarg)
  : res (list Clight.statement * compiler_state) :=
  match cs.(stack) with
  | T_num T_i32 :: rest =>
    let d := depth rest in
    do a       <- slot_expr (T_num T_i32) d;
    do dst     <- slot_ident (T_num ty) d;
    do res_ty  <- wasm_type_to_clight_type (T_num ty);
    do width   <- load_width ty trunc;
    do cell    <- load_cell ty trunc;
    do cell_ty <- cell_type cell;
    (* addr = (uint64_t)a + offset *)
    let addr := Clight.Ebinop Cop.Oadd (Clight.Ecast a tulong)
                  (const_u64 marg.(memarg_offset)) tulong in
    (* addr + width > mem->size *)
    let oob := Clight.Ebinop Cop.Ogt
                 (Clight.Ebinop Cop.Oadd addr (const_u64 (Z.to_N width)) tulong)
                 (mem_field mem_size tulong) tint in
    let src := Clight.Ecast
                 (Clight.Ebinop Cop.Oadd (mem_field mem_data (tptr tuchar))
                    addr (tptr tuchar))
                 (tptr tvoid) in
    let cell_ptr := Clight.Ecast
                      (Clight.Eaddrof (Clight.Evar cell cell_ty) (tptr cell_ty))
                      (tptr tvoid) in
    OK ([
         (* if (addr + w > mem->size) wasm_rt_trap(); *)
         Clight.Sifthenelse oob trap_stmt Clight.Sskip;
         (* memcpy(&cell, src, width) *)
         copy_data cell_ptr src width;
         (* dst = (cast)cell *)
         Clight.Sset dst (extend_cell trunc (Clight.Evar cell cell_ty) cell_ty res_ty)
        ],
        cs_push {| stack := rest; max_depth := cs.(max_depth) |} (T_num ty))
  | _ => Error (msg "load: expected i32 address on top of stack")
  end.

Definition compile_mem_instr (cs : compiler_state) (instr: basic_instruction)
  : res (list Clight.statement * compiler_state) :=
  match instr with
  | BI_load ty trunc marg      => compile_load cs ty trunc marg
  | BI_load_vec varg marg      => Error (msg "load_vec not supported")
  | BI_load_vec_lane vw ma li  => Error (msg "load_vec_lane not supported")
  | BI_store nt opt ma         => Error (msg "store not supported")
  | BI_store_vec ma            => Error (msg "store_vec not supported")
  | BI_store_vec_lane vw ma li => Error (msg "store_vec_lane not supported")
  | BI_memory_size             => Error (msg "memory_size not supported")
  | BI_memory_grow             => Error (msg "memory_grow not supported")
  | BI_memory_fill             => Error (msg "memory_fill not supported")
  | BI_memory_copy             => Error (msg "memory_copy not supported")
  | BI_memory_init idx         => Error (msg "memory_init not supported")
  | BI_data_drop idx           => Error (msg "data_drop not supported")
  | _ => Error (msg "not a memory instruction")
  end.
