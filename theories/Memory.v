From Wasm Require Import datatypes datatypes_properties operations numerics.
From Stdlib Require Import PArith NArith ZArith String List.
From compcert Require cfrontend.Clight cfrontend.Ctypes cfrontend.Cop common.AST common.Errors lib.Integers.
From compcert Require Import export.Ctypesdefs.
From Wasm2c Require Import Ident Util Instantiate Stack Extern.

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

Definition scratch_ptr : AST.ident := ident_of_scratch 6.

Definition scratch_vars : list (AST.ident * Ctypes.type) :=
  [(scratch_u8, tuchar); (scratch_u16, tushort); (scratch_u32, tuint);
   (scratch_u64, tulong); (scratch_f32, tfloat); (scratch_f64, tdouble)].

Definition scratch_temps : list (AST.ident * Ctypes.type) :=
  [(scratch_ptr, tptr tvoid)].

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
  | _, _ => Error (msg "invalid packed load/store")
  end.

(** which scratch cell to use based on store options *)
Definition store_cell (ty : number_type) (trunc : option packed_type) :
  res AST.ident :=
  match trunc with
  | None    => load_cell ty None
  | Some pt => load_cell ty (Some (pt, SX_U))
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
  | _, _ => Error (msg "invalid packed load/store")
  end.

(** width of store based on store options *)
Definition store_width (ty : number_type) (trunc : option packed_type)
  : res Z :=
  match trunc with
  | None    => load_width ty None
  | Some pt => load_width ty (Some (pt, SX_U))
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
         (* memcpy(&cell, src, width); *)
         copy_data cell_ptr src width;
         (* dst = (cast)cell; *)
         Clight.Sset dst (extend_cell trunc (Clight.Evar cell cell_ty) cell_ty res_ty)
        ],
        cs_push {| stack := rest; max_depth := cs.(max_depth) |} (T_num ty))
  | _ => Error (msg "load: expected i32 address on top of stack")
  end.

Definition compile_store (cs : compiler_state)
                         (ty : number_type)
                         (trunc : option packed_type)
                         (marg : memarg)
  : res (list Clight.statement * compiler_state) :=
  match cs.(stack) with
  | T_num ty' :: T_num T_i32 :: rest =>
    if negb (number_type_eqb ty ty') then
      Error (msg "store: value type does not match instruction")
    else
    let d := depth rest in
    do a       <- slot_expr (T_num T_i32) d;
    do dat     <- slot_expr (T_num ty) (N.succ d);
    do width   <- store_width ty trunc;
    do cell    <- store_cell ty trunc;
    do cell_ty <- cell_type cell;
    (* addr = (uint64_t)a + offset *)
    let addr := Clight.Ebinop Cop.Oadd (Clight.Ecast a tulong)
                 (const_u64 marg.(memarg_offset)) tulong in
    (* addr + width > mem->size *)
    let oob := Clight.Ebinop Cop.Ogt
                 (Clight.Ebinop Cop.Oadd addr (const_u64 (Z.to_N width)) tulong)
                 (mem_field mem_size tulong) tint in
    let dst := Clight.Ecast
                 (Clight.Ebinop Cop.Oadd (mem_field mem_data (tptr tuchar))
                   addr (tptr tuchar))
                 (tptr tvoid) in
    let cell_ptr := Clight.Ecast
                      (Clight.Eaddrof (Clight.Evar cell cell_ty) (tptr cell_ty))
                      (tptr tvoid) in
    OK ([
          (* if (addr + width > mem->size) wasm_rt_trap(); *)
          Clight.Sifthenelse oob trap_stmt Clight.Sskip;
          (* cell = (cell_type)data; *)
          Clight.Sassign (Clight.Evar cell cell_ty) (Clight.Ecast dat cell_ty);
          (* memcpy(mem->data + addr, &cell, width); *)
          copy_data dst cell_ptr width
        ],
        {| stack := rest; max_depth := cs.(max_depth) |})
  | _ => Error (msg "store: expected stack of shape store_type :: i32 :: ...")
    end.

Definition compile_memory_size (cs : compiler_state)
  : res (list Clight.statement * compiler_state) :=
  push_expr (T_num T_i32) cs (Clight.Ecast (mem_field mem_pages tulong) tuint).


Definition compile_memory_grow (cs : compiler_state)
  : res (list Clight.statement * compiler_state) :=
  match cs.(stack) with
  | T_num T_i32 :: rest =>
    let d := depth rest in
    do delta <- slot_expr (T_num T_i32) d;
    do slot  <- slot_ident (T_num T_i32) d;
    let delta64   := Clight.Ecast delta tulong in
    let pages     := mem_field mem_pages tulong in
    let page_size := const_u64 wasm_page_size in
    let p         := Clight.Etempvar scratch_ptr (tptr tvoid) in
    (* new_pages = mem->pages + (uint64_t)delta *)
    let new_pages := Clight.Ebinop Cop.Oadd pages delta64 tulong in
    (* new_pages > mem->max_pages *)
    let too_big := Clight.Ebinop Cop.Ogt new_pages
                    (mem_field mem_max_pages tulong) tint in
    (* delta == 0 *)
    let no_change := Clight.Ebinop Cop.Oeq delta
                      (Clight.Econst_int Integers.Int.zero tuint) tint in
    (* p == NULL *)
    let is_null := Clight.Ebinop Cop.Oeq p
                    (Clight.Ecast (Clight.Econst_int Integers.Int.zero tint) (tptr tvoid))
                    tint in
    (* slot = -1 *)
    let fail := Clight.Sset slot (Clight.Econst_int (Integers.Int.repr (-1)) tuint) in (* why must this be a tuint and not tint? *)
    (* slot = (uint32_t)mem->pages *)
    let push_pages := Clight.Sset slot (Clight.Ecast pages tuint) in
    (* p = realloc(mem->data, new_pages * 65536) *)
    let call_realloc :=
      Clight.Scall (Some scratch_ptr) (Clight.Evar ident_realloc trealloc)
        [Clight.Ecast (mem_field mem_data (tptr tuchar)) (tptr tvoid);
         Clight.Ebinop Cop.Omul new_pages page_size tulong] in
    (* memset((uint8_t * )p + mem->size, 0, (uint64_t)delta * 65536) *)
    let call_memset :=
      Clight.Scall None (Clight.Evar ident_memset tmemset)
        [Clight.Ecast
          (Clight.Ebinop Cop.Oadd (Clight.Ecast p (tptr tuchar))
            (mem_field mem_size tulong) (tptr tuchar))
          (tptr tvoid);
         Clight.Econst_int Integers.Int.zero tint;
         Clight.Ebinop Cop.Omul delta64 page_size tulong] in
    (* mem->data = (uint8_t * )p; *)
    do set_data <- set_mem_field mem_data (Clight.Ecast p (tptr tuchar));
    (* mem->pages = mem->pages + (uint64_t)delta *)
    do set_pages <- set_mem_field mem_pages new_pages;
    (* mem->size = mem->pages * 65536 *)
    do set_size <- set_mem_field mem_size
                    (Clight.Ebinop Cop.Omul pages page_size tulong);
    (* slot = (uint32_t)(mem_pages-> - (uint64_t)delta) *)
    let push_old :=
      Clight.Sset slot
        (Clight.Ecast (Clight.Ebinop Cop.Osub pages delta64 tulong) tuint) in
    let grow :=
      seq_of_list [
        call_realloc;
        Clight.Sifthenelse is_null fail
        (seq_of_list [call_memset; set_data; set_pages; set_size; push_old])
      ] in
    OK ([Clight.Sifthenelse too_big fail
          (Clight.Sifthenelse no_change push_pages grow)],
        cs) (* pops an i32 and pushes an i32 so unchanged stack *)
  | _ => Error (msg "memory.grow: expected i32 on top of stack")
  end.

Definition compile_mem_instr (cs : compiler_state) (instr : basic_instruction)
  : res (list Clight.statement * compiler_state) :=
  match instr with
  | BI_load ty trunc marg   => compile_load cs ty trunc marg
  | BI_load_vec _ _         => Error (msg "load_vec not supported in Wasm 1.0")
  | BI_load_vec_lane _ _ _  => Error (msg "load_vec_lane not supported in Wasm 1.0")
  | BI_store ty trunc marg  => compile_store cs ty trunc marg
  | BI_store_vec _          => Error (msg "store_vec not supported in Wasm 1.0")
  | BI_store_vec_lane _ _ _ => Error (msg "store_vec_lane not supported in Wasm 1.0")
  | BI_memory_size          => compile_memory_size cs
  | BI_memory_grow          => compile_memory_grow cs
  | BI_memory_fill          => Error (msg "memory_fill not supported in Wasm 1.0")
  | BI_memory_copy          => Error (msg "memory_copy not supported in Wasm 1.0")
  | BI_memory_init idx      => Error (msg "memory_init not supported in Wasm 1.0")
  | BI_data_drop idx        => Error (msg "data_drop not supported in Wasm 1.0")
  | _ => Error (msg "not a memory instruction")
  end.
