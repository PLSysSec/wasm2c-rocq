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
