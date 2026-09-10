From Wasm Require Import datatypes datatypes_properties operations numerics.
From Stdlib Require Import PArith NArith String List.
From compcert Require cfrontend.Clight cfrontend.Ctypes cfrontend.Cop common.AST common.Errors lib.Integers.
From compcert Require Import export.Ctypesdefs.

Import ListNotations.
Import Errors.

Local Open Scope error_monad_scope.

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
  | t :: rest => do t' <- wasm_type_to_clight_type t;
                 do rest' <- wasm_types_to_clight_types rest;
                 OK (t' :: rest')
  end.

(** turn a list of Wasm variables into a list of Clight variables. base is the 
    first fresh identifier *)
Fixpoint wasm_vars_to_clight_vars (base : N) (ts : list value_type) 
  : res (list (AST.ident * Ctypes.type)) :=
  match ts with
  | nil => OK nil
  | t :: ts' => do ty <- wasm_type_to_clight_type t;
                do rest <- wasm_vars_to_clight_vars (N.succ base) ts';
                OK (((ident_of_local base), ty) :: rest)
  end.

(** parameters start at identifier 0 *)
Definition wasm_params_to_clight_params (ts : list value_type) 
  : res (list (AST.ident * Ctypes.type)) :=
  wasm_vars_to_clight_vars 0 ts.

(** convert Wasm return type into Clight return type *)
Definition wasm_return_to_clight_return (ts : list value_type) 
  : res Ctypes.type :=
  match ts with
  | nil         => OK tvoid
  | t :: nil    => wasm_type_to_clight_type t
  | _ :: _ :: _ => Error (msg "multi-value return not supported")
  end.

(** split Wasm function_type into Clight return type, and parameters *)
Definition clight_of_functype (tf : function_type)
  : res (Ctypes.type * list (AST.ident * Ctypes.type)) :=
  let 'Tf ts1 ts2 := tf in
    do ret <- wasm_return_to_clight_return ts2;
    do ps <- wasm_params_to_clight_params ts1;
    OK (ret, ps)
  .

(** compiler state records the current stack and the max depth of the stack *)
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

(** turn a Wasm type + natural number into a Clight identifier *)
Definition slot_ident (t : value_type) (d : N) : res AST.ident :=
  match t with
  | T_num T_i32 => OK (ident_of_i32_slot d)
  | T_num T_i64 => OK (ident_of_i64_slot d)
  | T_num T_f32 => OK (ident_of_f32_slot d)
  | T_num T_f64 => OK (ident_of_f64_slot d)
  | T_ref _     => OK (ident_of_ref_slot d)
  | _ => Error (msg "unsupported stack slot type")
  end.

(** return a Clight expression for the variable at depth d in the stack. depth 0
    is the head, etc. *)
Definition slot_expr (t : value_type) (d : N) : res Clight.expr :=
  do id <- slot_ident t d;
  do ty <- wasm_type_to_clight_type t;
  OK (Clight.Etempvar id ty).

(** return a Clight statement representing a push to the stack *)
Definition push_expr (t : value_type) (cs : compiler_state) (e : Clight.expr)
  : res (list Clight.statement * compiler_state) :=
  do id <- slot_ident t (depth cs.(stack));
  OK ([Clight.Sset id e], cs_push cs t).

(** convert a single Wasm basic_instruction to 1+ Clight statements *)
Definition instr_to_statement (cs : compiler_state) (instr : basic_instruction) 
  : res (list Clight.statement * compiler_state) :=
  match instr with
  | BI_const_num val => 
    match val with
    | VAL_int32 num   => push_expr (T_num T_i32) cs (Clight.Econst_int (Integers.Int.repr (Wasm_int.Z_of_uint i32m num)) tuint)
    | VAL_int64 num   => push_expr (T_num T_i64) cs (Clight.Econst_long (Integers.Int64.repr (Wasm_int.Z_of_uint i64m num)) tulong)
    | VAL_float32 num => push_expr (T_num T_f32) cs (Clight.Econst_single num tfloat)
    | VAL_float64 num => push_expr (T_num T_f64) cs (Clight.Econst_float num tdouble)
    end
  | BI_binop T_i32 (Binop_i op') =>
    match op', cs.(stack) with 
    | BOI_add, T_num T_i32 :: T_num T_i32 :: rest =>
      let d2 := depth rest in
        do id2 <- slot_ident (T_num T_i32) d2;
        do e1  <- slot_expr  (T_num T_i32) d2;
        do e2  <- slot_expr  (T_num T_i32) (N.succ d2);
        OK (
          [Clight.Sset id2 (Clight.Ebinop Cop.Oadd e1 e2 tuint)],
          cs_push {| stack := rest; max_depth := cs.(max_depth) |} (T_num T_i32)
        )
    | _, _ => Error (msg "unsupported i32 binary operation")
    end
  | _ => Error (msg "unsupported instruction")
  end.

(** turn a list of Clight statements into a single statement using Ssequence *)
Definition seq_of_list (l : list Clight.statement) : Clight.statement :=
  List.fold_right Clight.Ssequence Clight.Sskip l.

(** turn a list of Wasm instructions into a list of Clight statements*)
Fixpoint instrs_to_statements 
  (cs : compiler_state) 
  (body : list basic_instruction)
  : res (list Clight.statement * compiler_state) :=
  match body with
  | nil => OK (nil, cs)
  | instr :: body' =>
    do (ss, cs') <- instr_to_statement cs instr;
    do (ss', cs'') <- instrs_to_statements cs' body';
    OK (ss ++ ss', cs'')
  end.

(** default compiler state has empty stack *)
Definition cs_initial : compiler_state := {|
  stack := nil; max_depth := 0
|}.

(** turn Wasm return type into Clight return statement *)
Definition return_stmt (ret_type : list value_type) (cs : compiler_state)
  : res Clight.statement :=
  match ret_type, cs.(stack) with
  | nil, nil => OK (Clight.Sreturn None)
  | t :: nil, t' :: rest =>
    if value_type_eqb t t' then
      do e <- slot_expr t (depth rest);
      OK (Clight.Sreturn (Some e))
    else Error (msg "stack and return type mismatch")
  | nil, _ :: _ => Error (msg "function returns nothing but values are left on the operand stack")
  | _ :: nil, nil => Error (msg "function must return a value but the operand stack is empty")
  | _ :: _ :: _, _ => Error (msg "multi-value returns are not supported")
  end.

(** compile the body of a Wasm function into a Clight statement *)
Definition compile_body (ret_type : list value_type) (body : expr) 
  : res (Clight.statement * compiler_state) :=
  do (ss, cs) <- instrs_to_statements cs_initial body;
  do ret <- return_stmt ret_type cs;
  OK (seq_of_list (ss ++ [ret]), cs).

(** generate a list of h Clight temps of a given type *)
Definition slot_temps (mk : N -> AST.ident) (ty : Ctypes.type) (h : N)
  : list (AST.ident * Ctypes.type) :=
  List.map (fun d => (mk (N.of_nat d), ty)) (List.seq 0 (N.to_nat h)).

(** compile a Wasm function. Note: module_func defined in 
    WasmCert-Coq/theories/datatypes.v:639; Clight.function defined in 
    CompCert/cfrontend/Clight.v:135 *)
Definition compile_func (m : module) (func : module_func) 
  : res Clight.function :=
  match lookup_N m.(mod_types) func.(modfunc_type) with
  | Some (Tf ts1 ts2 as func_type) => 
    do (ret_type, params) <- clight_of_functype func_type;
    do locals <- wasm_vars_to_clight_vars (N.of_nat (List.length ts1)) func.(modfunc_locals);
    do (body, cs) <- compile_body ts2 func.(modfunc_body);
    let stack_temps := (
      slot_temps ident_of_i32_slot tuint cs.(max_depth) ++
      slot_temps ident_of_i64_slot tulong cs.(max_depth) ++
      slot_temps ident_of_f32_slot tfloat cs.(max_depth) ++
      slot_temps ident_of_f64_slot tdouble cs.(max_depth) ++
      slot_temps ident_of_ref_slot (tptr tvoid) cs.(max_depth)
    ) in
      OK (
        Clight.mkfunction ret_type AST.cc_default params nil (params ++ locals ++ stack_temps) body
      )
  | None => Error (msg "function type couldn't be found in binary")
end.

(* turn a Wasm name into a string *)
Definition string_of_name (n : name) : String.string :=
  String.string_of_list_byte n.

(** compile a list of Wasm imported functions into a list of Clight external 
    functions. idx is the ident number to start at *)
Fixpoint compile_func_imports (m : module) (idx : N) (imps : list module_import)
  : res (list (AST.ident * AST.globdef Clight.fundef Ctypes.type)) :=
  match imps with 
  | nil => OK nil
  | imp :: rest =>
    match imp.(imp_desc) with
    | MID_func tidx =>
      match lookup_N m.(mod_types) tidx with
      | Some (Tf ts1 ts2) =>
        do args <- wasm_types_to_clight_types ts1;
        do ret <- wasm_return_to_clight_return ts2;
        do rest' <- compile_func_imports m (N.succ idx) rest;
        let sg := Ctypes.signature_of_type args ret AST.cc_default in
          OK (
              (ident_of_func idx,
               AST.Gfun (Ctypes.External
                        (AST.EF_external (string_of_name imp.(imp_name)) sg) 
                        args ret AST.cc_default)
              ) :: rest'
          )
      | None => Error (msg "imported function type can't be found")
      end
    | _ => compile_func_imports m idx rest (* if it's not a function just skip *)
    end
  end.

(** compile Wasm functions into Clight functions, assigning identifiers starting 
    from idx *)
Fixpoint compile_funcs_from (m : module) (idx : N) (funcs : list module_func)
  : res (list (AST.ident * AST.globdef Clight.fundef Ctypes.type)) :=
  match funcs with
  | nil => OK nil
  | f :: rest =>
    do cf <- compile_func m f;
    do rest' <- compile_funcs_from m (N.succ idx) rest;
    OK ((ident_of_func idx, AST.Gfun (Ctypes.Internal cf)) :: rest')
  end.

(** count the number of imported functions in a module *)
Definition n_imported_functions (m : module) : N :=
  N.of_nat (List.length (List.filter
    (fun imp => match imp.(imp_desc) with MID_func _ => true | _ => false end)
    m.(mod_imports))).

(** compile the functions from a Wasm module into Clight *)
Definition compile_funcs (m : module)
  : res (list (AST.ident * AST.globdef Clight.fundef Ctypes.type)) :=
  (* start internal functions after the external calls *)
  compile_funcs_from m (n_imported_functions m) m.(mod_funcs).


(** Note: module defined in WasmCert-Coq/theories/datatypes.v:740; 
    Clight.program defined in CompCert/cfrontend/Ctypes.v:1545 *)
Definition compile (m : module) : Errors.res Clight.program :=
  do imports <- compile_func_imports m 0 m.(mod_imports);
  do defs <- compile_funcs m;
  Ctypes.make_program nil (imports ++ defs) nil 1%positive.
