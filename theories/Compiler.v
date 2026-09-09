From Wasm Require Import datatypes datatypes_properties operations numerics.
From Stdlib Require Import PArith NArith String List.
From compcert Require cfrontend.Clight cfrontend.Ctypes cfrontend.Cop common.AST common.Errors lib.Integers.
From compcert Require Import export.Ctypesdefs.

Import ListNotations.
Import Errors.

(* tag type *)
Definition ident_of_func     (i : N) : AST.ident := ((N.succ_pos i)~0~0~0)%positive.
Definition ident_of_global   (i : N) : AST.ident := ((N.succ_pos i)~0~0~1)%positive.
Definition ident_of_local    (i : N) : AST.ident := ((N.succ_pos i)~0~1~0)%positive.
Definition ident_of_i32_slot (i : N) : AST.ident := ((N.succ_pos i)~0~1~1)%positive.
Definition ident_of_i64_slot (i : N) : AST.ident := ((N.succ_pos i)~1~0~0)%positive.
Definition ident_of_f32_slot (i : N) : AST.ident := ((N.succ_pos i)~1~0~1)%positive.
Definition ident_of_f64_slot (i : N) : AST.ident := ((N.succ_pos i)~1~1~0)%positive.
Definition ident_of_ref_slot (i : N) : AST.ident := ((N.succ_pos i)~1~1~1)%positive.

Definition wasm_type_to_clight_type (t : value_type) : res Ctypes.type :=
  match t with
  | T_num T_i32  => OK tuint        (* always 32 bits *)
  | T_num T_i64  => OK tulong       (* always 64 bits *)
  | T_num T_f32  => OK tfloat
  | T_num T_f64  => OK tdouble
  | T_ref _      => OK (tptr tvoid) (* don't care if it's a funcref or extern ref *)
  | T_vec T_v128 => Error (msg "No Clight equivalent for T_vec T_v128") 
  | T_bot        => Error (msg "No Clight equivalent for T_bot")
  end.

Fixpoint wasm_types_to_clight_types (ts : list value_type) 
  : res (list Ctypes.type) :=
  match ts with
  | nil => OK nil
  | t :: rest =>
    match wasm_type_to_clight_type t, wasm_types_to_clight_types rest with
    | OK t', OK rest' => OK (t' :: rest')
    | Error err, _ => Error err
    | _, Error err => Error err
    end
  end.

Fixpoint wasm_locals_to_clight_locals (base : N) (ts : list value_type) 
  : res (list (AST.ident * Ctypes.type)) :=
  match ts with
  | nil => OK nil
  | t :: ts' =>
    match wasm_type_to_clight_type t,
          wasm_locals_to_clight_locals (N.succ base) ts' with
    | OK ty, OK rest =>  OK (((ident_of_local base), ty) :: rest)
    | Error err, _ => Error err
    | _, Error err => Error err
    end
  end.

Definition wasm_params_to_clight_params (ts : list value_type) 
  : res (list (AST.ident * Ctypes.type)) :=
  wasm_locals_to_clight_locals 0 ts.

Definition wasm_return_to_clight_return (ts : list value_type) 
  : res Ctypes.type :=
  match ts with
  | nil      => OK tvoid
  | t :: nil => wasm_type_to_clight_type t
  | _ :: _ :: _ => Error (msg "multi-value return not supported")
  end.

(* return type, calling_convention, params *)
Definition clight_of_functype (tf : function_type)
  : res (
    Ctypes.type * 
    AST.calling_convention * 
    list (AST.ident * Ctypes.type)
  ) :=
  let 'Tf ts1 ts2 := tf in
    match wasm_return_to_clight_return ts2, 
          wasm_params_to_clight_params ts1 with
    | OK ret, OK ps => OK (ret, AST.cc_default, ps)
    | Error err, _ => Error err
    | _, Error err => Error err
    end
  .

Record compiler_state : Type := {
  stack : list value_type; (* head is the top of the stack *)
  max_depth : N
}.

Definition depth (s : list value_type) : N := N.of_nat (List.length s).

Definition cs_push (cs : compiler_state) (t : value_type) : compiler_state :=
  {| 
    stack := t :: cs.(stack);
    max_depth := N.max cs.(max_depth) (N.succ (depth cs.(stack)))
  |}.

Definition slot_ident (t : value_type) (d : N) : res AST.ident :=
  match t with
  | T_num T_i32 => OK (ident_of_i32_slot d)
  | T_num T_i64 => OK (ident_of_i64_slot d)
  | T_num T_f32 => OK (ident_of_f32_slot d)
  | T_num T_f64 => OK (ident_of_f64_slot d)
  | T_ref _     => OK (ident_of_ref_slot d)
  | _ => Error (msg "unsupported stack slot type")
  end.

(* return a Clight expr representing depth N of the stack*)
Definition slot_expr (t : value_type) (d : N) : res Clight.expr :=
  match slot_ident t d, wasm_type_to_clight_type t with
  | OK id, OK ty => OK (Clight.Etempvar id ty)
  | Error err, _ => Error err
  | _, Error err => Error err
  end.

Definition push_const (t : value_type) (cs : compiler_state) (e : Clight.expr)
  : res (list Clight.statement * compiler_state) :=
  match slot_ident t (depth cs.(stack)) with
  | OK id => OK ([Clight.Sset id e], cs_push cs t)
  | Error err => Error err
  end.

Definition instr_to_statement (cs : compiler_state) (instr : basic_instruction) 
  : res (list Clight.statement * compiler_state) :=
  match instr with
  | BI_const_num val => 
    match val with
    | VAL_int32 num   => push_const (T_num T_i32) cs (Clight.Econst_int (Integers.Int.repr (Wasm_int.Z_of_uint i32m num)) tuint)
    | VAL_int64 num   => push_const (T_num T_i64) cs (Clight.Econst_long (Integers.Int64.repr (Wasm_int.Z_of_uint i64m num)) tulong)
    | VAL_float32 num => push_const (T_num T_f32) cs (Clight.Econst_single num tfloat)
    | VAL_float64 num => push_const (T_num T_f64) cs (Clight.Econst_float num tdouble)
    end
  | BI_binop T_i32 (Binop_i op') =>
    match op', cs.(stack) with 
    | BOI_add, T_num T_i32 :: T_num T_i32 :: rest =>
      let d2 := depth rest in
        match slot_ident (T_num T_i32) d2,
              slot_expr  (T_num T_i32) d2,
              slot_expr  (T_num T_i32) (N.succ d2) with
        | OK id2, OK e1, OK e2 =>
          OK ([Clight.Sset id2 (Clight.Ebinop Cop.Oadd e1 e2 tuint)],
                cs_push {| stack := rest; max_depth := cs.(max_depth) |} (T_num T_i32))
        | Error err, _, _ => Error err
        | _, Error err, _ => Error err
        | _, _, Error err=> Error err
        end
    | _, _ => Error (msg "unsupported i32 binary operation")
    end
  | _ => Error (msg "unsupported instruction")
  end.

Definition seq_of_list (l : list Clight.statement) : Clight.statement :=
  List.fold_right Clight.Ssequence Clight.Sskip l.

Fixpoint instrs_to_statements 
  (cs : compiler_state) 
  (body : list basic_instruction)
  : res (list Clight.statement * compiler_state) :=
  match body with
  | nil => OK (nil, cs)
  | instr :: body' =>
    match instr_to_statement cs instr with
    | OK (ss, cs') =>
      match instrs_to_statements cs' body' with
      | OK (ss', cs'') => OK (ss ++ ss', cs'')
      | Error err => Error err
      end
    | Error err => Error err
    end
  end.

Definition cs_initial : compiler_state := {|
  stack := nil; max_depth := 0
|}.

Definition return_stmt (ret_type : list value_type) (cs : compiler_state)
  : res Clight.statement :=
  match ret_type, cs.(stack) with
  | nil, nil => OK (Clight.Sreturn None)
  | t :: nil, t' :: rest =>
    if value_type_eqb t t' then
      match slot_expr t (depth rest) with
      | OK e => OK (Clight.Sreturn (Some e))
      | Error err => Error err
      end
    else Error (msg "stack and return type mismatch")
  | nil, _ :: _ => Error (msg "function returns nothing but values are left on the operand stack")
  | _ :: nil, nil => Error (msg "function must return a value but the operand stack is empty")
  | _ :: _ :: _, _ => Error (msg "multi-value returns are not supported")
  end.

Definition compile_body (ret_type : list value_type) (body : expr) 
  : res (Clight.statement * compiler_state) :=
  match instrs_to_statements cs_initial body with
  | OK (ss, cs) => 
    match return_stmt ret_type cs with
    | OK ret => OK (seq_of_list (ss ++ [ret]), cs)
    | Error err => Error err
    end
  | Error err => Error err
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

Definition slot_temps (mk : N -> AST.ident) (ty : Ctypes.type) (h : N)
  : list (AST.ident * Ctypes.type) :=
  List.map (fun d => (mk (N.of_nat d), ty)) (List.seq 0 (N.to_nat h)).

(*
    module_func defined in WasmCert-Coq/theories/datatypes.v:639
    Clight.function defined in CompCert/cfrontend/Clight.v:135
*)
Definition compile_func (m : module) (func : module_func) 
  : res Clight.function :=
  match lookup_N m.(mod_types) func.(modfunc_type) with
  | Some (Tf ts1 ts2 as func_type) =>
    match clight_of_functype func_type with
    | OK (ret_type, cc, params) =>
      (* local idxs start after params *)
      match wasm_locals_to_clight_locals (N.of_nat (List.length ts1))
                                         func.(modfunc_locals) with
      | OK locals =>
        match compile_body ts2 func.(modfunc_body) with
        | OK (body, cs) =>
          let stack_temps := (
            slot_temps ident_of_i32_slot tuint cs.(max_depth) ++
            slot_temps ident_of_i64_slot tulong cs.(max_depth) ++
            slot_temps ident_of_f32_slot tfloat cs.(max_depth) ++
            slot_temps ident_of_f64_slot tdouble cs.(max_depth) ++
            slot_temps ident_of_ref_slot (tptr tvoid) cs.(max_depth)
          ) in
            OK (
              Clight.mkfunction ret_type cc params nil (params ++ locals ++ stack_temps) body
            )
        | Error err => Error err
        end
      | Error err => Error err
      end
    | Error err => Error err
    end
  | None => Error (msg "function type couldn't be found in binary")
end.

Definition string_of_name (n : name) : String.string :=
  String.string_of_list_byte n.

Fixpoint compile_func_imports (m : module) (idx : N) (imps : list module_import)
  : res (list (AST.ident * AST.globdef Clight.fundef Ctypes.type)) :=
  match imps with 
  | nil => OK nil
  | imp :: rest =>
    match imp.(imp_desc) with
    | MID_func tidx =>
      match lookup_N m.(mod_types) tidx with
      | Some (Tf ts1 ts2) =>
        (* external function types take list of types, not list of (ident * type) *)
        match wasm_types_to_clight_types ts1,
              wasm_return_to_clight_return ts2,
              compile_func_imports m (N.succ idx) rest with
        | OK args, OK ret, OK rest' =>
          let sg := Ctypes.signature_of_type args ret AST.cc_default in
          OK (
            (ident_of_func idx,
            AST.Gfun (Ctypes.External
                      (AST.EF_external (string_of_name imp.(imp_name)) sg) 
                      args ret AST.cc_default)) :: rest'
          )
        | Error err, _, _ => Error err
        | _, Error err, _ => Error err
        | _, _, Error err => Error err
        end
      | None => Error (msg "imported function type can't be found")
      end
    | _ => compile_func_imports m idx rest (* if it's not a function just skip *)
    end
  end.

Fixpoint compile_funcs_from (m : module) (idx : N) (funcs : list module_func)
  : res (list (AST.ident * AST.globdef Clight.fundef Ctypes.type)) :=
  match funcs with
  | nil => OK nil
  | f :: rest =>
    match compile_func m f, compile_funcs_from m (N.succ idx) rest with
    | OK cf, OK rest' =>
      OK ((ident_of_func idx, AST.Gfun (Ctypes.Internal cf)) :: rest')
    | Error err, _ => Error err
    | _, Error err => Error err
    end
  end.

Definition n_imported_functions (m : module) : N :=
  N.of_nat (List.length (List.filter
    (fun imp => match imp.(imp_desc) with MID_func _ => true | _ => false end)
    m.(mod_imports))).

Definition compile_funcs (m : module)
  : res (list (AST.ident * AST.globdef Clight.fundef Ctypes.type)) :=
  (* start internal functions after the external calls *)
  compile_funcs_from m (n_imported_functions m) m.(mod_funcs).


(*
    - module defined in WasmCert-Coq/theories/datatypes.v:740
    - Clight.program defined in CompCert/cfrontend/Ctypes.v:1545 (res discharges
      a proof obligation)
*)
Definition compile (m : module) : Errors.res Clight.program :=
  match compile_func_imports m 0 m.(mod_imports), compile_funcs m with
  | OK imports, OK defs =>
    Ctypes.make_program nil (imports ++ defs) nil 1%positive
  | Error err, _ => Error err
  | _, Error err => Error err
  end.

