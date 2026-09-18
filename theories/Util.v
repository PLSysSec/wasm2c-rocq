From Wasm Require Import datatypes.
From Stdlib Require Import ZArith NArith String List.
From compcert Require cfrontend.Clight.
From compcert Require Import export.Ctypesdefs.
From compcert Require common.Errors.
From Wasm2c Require Import Ident.
Import Errors.
Local Open Scope error_monad_scope.

(** count the number of imported functions in a module *)
Definition n_imported_functions (m : module) : N :=
  N.of_nat (List.length (List.filter
    (fun imp => match imp.(imp_desc) with MID_func _ => true | _ => false end)
    m.(mod_imports))).

(** count the number of imported tables in a module *)
Definition n_imported_tables (m : module) : N :=
  N.of_nat (List.length (List.filter
    (fun imp => match imp.(imp_desc) with MID_table _ => true | _ => false end)
    m.(mod_imports))).

(** count the number of imported memories in a module *)
Definition n_imported_memories (m : module) : N :=
  N.of_nat (List.length (List.filter
    (fun imp => match imp.(imp_desc) with MID_mem _ => true | _ => false end)
    m.(mod_imports))).

(** count the number of imported globals in a module *)
Definition n_imported_globals (m : module) : N :=
  N.of_nat (List.length (List.filter
    (fun imp => match imp.(imp_desc) with MID_global _ => true | _ => false end)
    m.(mod_imports))).

(** count the number of defined memories in a module *)
Definition n_defined_memories (m : module) : N :=
  N.of_nat (List.length m.(mod_mems)).

(** turn a Wasm name into a string *)
Definition string_of_name (n : name) : String.string :=
  String.string_of_list_byte n.

(** turn N into const long expr -- unsignedness represented bc it's a tulong *)
Definition const_u64 (n : N) : Clight.expr :=
  Clight.Econst_long (Integers.Int64.repr (Z.of_N n)) tulong.

(** turn a list of Clight statements into a single statement using Ssequence *)
Definition seq_of_list (l : list Clight.statement) : Clight.statement :=
  List.fold_right Clight.Ssequence Clight.Sskip l.

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

(** normal parameters + Wasm instance pointer *)
Definition wasm_params_to_clight_params (ts : list value_type)
  : res (list (AST.ident * Ctypes.type)) :=
  do params <- wasm_vars_to_clight_vars 0 ts;
  OK ((ident_inst, tinst_ptr) :: params).

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
    OK (ret, ps).

(** the Clight function-pointer type for a Wasm function type -- mirrors
    exactly how compile_func builds a function's signature: the instance
    pointer, then the wasm params *)
Definition func_ptr_type (tf : function_type) : res Ctypes.type :=
  do (ret, params) <- clight_of_functype tf;
  OK (Ctypes.Tfunction (List.map snd params) ret AST.cc_default).

(** bijective base-6 encoding of a value_type list,
    used to build a call_indirect signature tag *)
Fixpoint value_types_tag (ts : list value_type) : res N :=
  match ts with
  | nil => OK 0%N
  | t :: rest =>
    do d <- (match t with
             | T_num T_i32 =>   OK 1%N
             | T_num T_i64 =>   OK 2%N
             | T_num T_f32 =>   OK 3%N
             | T_num T_f64 =>   OK 4%N
             | T_vec T_v128 =>  OK 5%N
             | T_ref _     =>   OK 6%N
             | _ => Error (msg "unsupported value type in call_indirect signature")
             end);
    do rest_tag <- value_types_tag rest;
    OK (d + 6 * rest_tag)%N
  end.

(** exact pairing N*N -> N (Cantor pairing), so a param-list tag and a
    return-list tag can never bleed into each other regardless of length *)
Definition pair_tags (a b : N) : N :=
  (((a + b) * (a + b + 1)) / 2 + b)%N.

(** canonical, module-independent signature tag for a Wasm function type *)
Definition signature_tag (tf : function_type) : res N :=
  let 'Tf ts1 ts2 := tf in
  do p1 <- value_types_tag ts1;
  do p2 <- value_types_tag ts2;
  OK (pair_tags p1 p2).
