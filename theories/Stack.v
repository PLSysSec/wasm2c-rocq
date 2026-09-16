From Wasm Require Import datatypes.
From Stdlib Require Import NArith String List.
From compcert Require cfrontend.Clight common.AST common.Errors.
From Wasm2c Require Import Ident Util.

Import ListNotations.
Import Errors.

Local Open Scope error_monad_scope.

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
    is the bottom, etc. *)
Definition slot_expr (t : value_type) (d : N) : res Clight.expr :=
  do id <- slot_ident t d;
  do ty <- wasm_type_to_clight_type t;
  OK (Clight.Etempvar id ty).

(** return a Clight statement representing a push to the stack *)
Definition push_expr (t : value_type) (cs : compiler_state) (e : Clight.expr)
  : res (list Clight.statement * compiler_state) :=
  do id <- slot_ident t (depth cs.(stack));
  OK ([Clight.Sset id e], cs_push cs t).