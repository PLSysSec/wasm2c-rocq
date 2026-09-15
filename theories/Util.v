From Wasm Require Import datatypes.
From Stdlib Require Import ZArith NArith String List.
From compcert Require cfrontend.Clight.
From compcert Require Import export.Ctypesdefs.
From compcert Require common.Errors.
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

(** turn Z into const long expr -- unsignedness represented bc it's a tulong *)
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
