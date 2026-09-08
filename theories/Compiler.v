From Wasm Require Import datatypes operations numerics.
From Stdlib Require Import PArith.
From compcert Require cfrontend.Clight cfrontend.Ctypes cfrontend.Cop common.AST common.Errors lib.Integers.
From compcert Require Import export.Ctypesdefs.
Require Import List.
Import ListNotations.

(* tag type *)
Definition ident_of_func   (i : N) : AST.ident := (N.succ_pos i)~0~0.
Definition ident_of_global (i : N) : AST.ident := (N.succ_pos i)~0~1.
Definition ident_of_local  (i : N) : AST.ident := (N.succ_pos i)~1~0.
Definition ident_of_slot   (i : N) : AST.ident := (N.succ_pos i)~1~1.

Definition wasm_type_to_clight_type (t : value_type) : option Ctypes.type :=
  match t with
  | T_num T_i32  => Some tuint        (* always 32 bits *)
  | T_num T_i64  => Some tulong       (* always 64 bits *)
  | T_num T_f32  => Some tfloat
  | T_num T_f64  => Some tdouble
  | T_ref _      => Some (tptr tvoid) (* don't care if it's a funcref or extern ref *)
  | T_vec T_v128 => None              (* no scalar equivalent *)
  | T_bot        => None
  end.

Fixpoint wasm_locals_to_clight_locals (base : N) (ts : list value_type) 
  : option (list (AST.ident * Ctypes.type)) :=
  match ts with
  | nil => Some nil
  | t :: ts' =>
    match wasm_type_to_clight_type t,
          wasm_locals_to_clight_locals (N.succ base) ts' with
    | Some ty, Some rest =>  Some (((ident_of_local base), ty) :: rest)
    | _, _ => None
    end
  end.

Fixpoint wasm_params_to_clight_params (ts : list value_type) 
  : option (list (AST.ident * Ctypes.type)) :=
  wasm_locals_to_clight_locals 0 ts.

Definition wasm_return_to_clight_return (ts : list value_type) 
  : option Ctypes.type :=
  match ts with
  | nil      => Some tvoid
  | t :: nil => wasm_type_to_clight_type t
  | _ :: _ :: _ => None (* multi-value (i think can't happen in wasm 1.0) *)
  end.

(* return type, calling_convention, params *)
Definition clight_of_functype (tf : function_type)
  : option (
    Ctypes.type * 
    AST.calling_convention * 
    list (AST.ident * Ctypes.type)
  ) :=
  let 'Tf ts1 ts2 := tf in
    match wasm_return_to_clight_return ts2, 
          wasm_params_to_clight_params ts1 with
    | Some ret, Some ps => Some (ret, AST.cc_default, ps)
    | _, _ => None
    end
  .

Record compiler_state : Type := {
  next : nat
}.

Definition increment_csnext (cs : compiler_state) : compiler_state :=
  {| next := S cs.(next) |}.

Definition decrement_csnext (cs : compiler_state) : compiler_state :=
  {| next := pred cs.(next) |}.

(* stack slot number -> Clight identifier *)
Definition slot_ident (n : nat) : AST.ident := Pos.of_succ_nat n.

Definition instr_to_statement (cs : compiler_state) (instr : basic_instruction) 
  : option (list Clight.statement * compiler_state) :=
  match instr with
  | BI_const_num val => match val with
    | VAL_int32 num => Some ([Clight.Sassign (Clight.Evar (slot_ident cs.(next)) tuint)
                                (Clight.Econst_int (Integers.Int.repr (Wasm_int.Z_of_uint i32m num)) tuint)], increment_csnext cs)
    | VAL_int64 num => Some ([Clight.Sassign (Clight.Evar (slot_ident cs.(next)) tulong)
                                (Clight.Econst_long (Integers.Int64.repr (Wasm_int.Z_of_uint i64m num)) tulong)], increment_csnext cs)
    | VAL_float32 num => Some ([Clight.Sassign (Clight.Evar (slot_ident cs.(next)) tfloat)
                                (Clight.Econst_single num tfloat)], increment_csnext cs)
    | VAL_float64 num => Some ([Clight.Sassign (Clight.Evar (slot_ident cs.(next)) tdouble)
                                (Clight.Econst_float num tdouble)], increment_csnext cs)
    end
  | BI_binop _ op => match op with
    | Binop_i op' => match op' with
      | BOI_add => Some ([Clight.Sassign (Clight.Evar (slot_ident (cs.(next) - 2)) tuint)
                            (Clight.Ebinop Cop.Oadd (Clight.Evar (slot_ident (cs.(next) - 2)) tuint)
                                                    (Clight.Evar (slot_ident (cs.(next) - 1)) tuint) tuint)], decrement_csnext cs)
      | BOI_sub => Some ([Clight.Sassign (Clight.Evar (slot_ident (cs.(next) - 2)) tuint)
                            (Clight.Ebinop Cop.Osub (Clight.Evar (slot_ident (cs.(next) - 2)) tuint)
                                                    (Clight.Evar (slot_ident (cs.(next) - 1)) tuint) tuint)], decrement_csnext cs)
      | _ => None
      end
    | _ => None
    end
  (* everything else, including the simd instructions that have no counterpart
     in vanilla compcert, is not translated yet *)
  | _ => None
  end.


Definition seq_of_list (l : list Clight.statement) : Clight.statement :=
  List.fold_right Clight.Ssequence Clight.Sskip l.

Fixpoint instrs_to_statements 
  (cs : compiler_state) 
  (body : list basic_instruction)
  : option (list Clight.statement * compiler_state) :=
  match body with
  | nil => Some (nil, cs)
  | instr :: body' =>
    match instr_to_statement cs instr with
    | Some (ss, cs') =>
      match instrs_to_statements cs' body' with
      | Some (ss', cs'') => Some (ss ++ ss', cs'')
      | None => None
      end
    | None => None
    end
  end.

Definition compile_body (body : expr) : option Clight.statement :=
  match instrs_to_statements {| next := 0 |} body with
  | Some (ss, _) => Some (seq_of_list ss)
  | None => None
  end.

(*  Clight.function := { 
      fn_return: type;                // Can figure out from function type
      fn_callconv: calling_convention := {
        cc_vararg: option Z; // variable args? I think should be None
        cc_unproto: bool;    // <<old-style unprototyped function>> I assume just false
        cc_structret: bool   // I think never returns a struct? Unless multi return
      }
      fn_params: list (ident * type); // Types can come from function type, idents come from stack?
      fn_vars: list (ident * type);   // Addressable local variables -- [] I think bc wasm locals non-addressable
      fn_temps: list (ident * type);  // Non-addressable local variables func.(modfunc_locals) i think
      fn_body: statement              // func.(modfunc_body)
    }

    func_type := lookup_N m.(mod_types) func.(modfunc_type)
*)


(*
    module_func defined in WasmCert-Coq/theories/datatypes.v:639
    Clight.function defined in CompCert/cfrontend/Clight.v:135
*)
Definition compile_func (cs : compiler_state) (m : module) (func : module_func) 
  : option (Clight.function * compiler_state) :=
  match lookup_N m.(mod_types) func.(modfunc_type) with
  | Some (Tf ts1 ts2 as func_type) =>
    match clight_of_functype func_type with
    | Some (ret_type, cc, params) =>
      (* local idxs start after params *)
      match wasm_locals_to_clight_locals (N.of_nat (List.length ts1))
                                         func.(modfunc_locals) with
      | Some locals =>
        match compile_body func.(modfunc_body) with
        | Some body =>
          Some (
            Clight.mkfunction ret_type cc params nil (params ++ locals) body
          )
        | _ => None
        end
      | _ => None
      end
    | _ => None
    end
  | _ => None
end.

Definition compile_funcs 
  (cs : compiler_state) 
  (m : module)
  : option (list (Clight.function) * compiler_state) :=
  let funcs := m.(mod_funcs) in 
    match funcs with
    | nil => Some (nil, cs)
    | f :: rest =>
      match compile_func cs m f with
      | Some (cfunc, cs') =>
        match compile_funcs cs' m rest with
        | Some (cfuncs, cs'') => Some (cfunc :: cfuncs, cs'')
        | None => None
        end
      | None => None
      end
    end
  .

Definition cs_initial : compiler_state := {|
  next := 0
|}

(*
    - module defined in WasmCert-Coq/theories/datatypes.v:740
    - Clight.program defined in CompCert/cfrontend/Ctypes.v:1545 (res discharges
      a proof obligation)
*)
Definition compile (m : module) : Errors.res Clight.program :=
  match compile_funcs cs_initial m with
  | Some (cfuncs, cs') =>
  (* need to provide these as fundefs *)

  Ctypes.make_program nil nil nil 1%positive.

(*
make_program (types: list composite_definition) 
             (defs: list (ident * globdef fundef type))
             (public: list ident)
             (main: ident) // what about programs with no main func?
  : res program
*)
