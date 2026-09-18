From Wasm Require Import datatypes datatypes_properties operations numerics.
From compcert Require cfrontend.Clight cfrontend.Ctypes cfrontend.Cop common.AST common.Errors.
From compcert Require Import export.Ctypesdefs.
From Wasm2c Require Import Ident Util Instantiate Stack.

Definition table_get (cs : compiler_state) (idx : tuint)
  : res (list Clight.statement * compiler_state) :=

  Error (msg "Todo").

Definition table_set (cs : compiler_state) (idx : tuint)
  : res (list Clight.statement * compiler_state) :=
  Error (msg "Todo").

Definition table_size (cs : compiler_state) (idx : tuint)
  : res (list Clight.statement * compiler_state) :=
  push_expr (T_num T_i32) cs (Clight.Ecast (table_field table_size) tuint).

Definition table_grow (cs : compiler_state) (idx : tuint)
  : res (list Clight.statement * compiler_state) :=
  Error (msg "Todo").

Definition table_fill (cs : compiler_state) (idx : tuint)
  : res (list Clight.statement * compiler_state) :=
  Error (msg "Todo").

Definition table_copy (cs : compiler_state) (idx1 : tuint) (idx2 : tuint)
  : res (list Clight.statement * compiler_state) :=
  Error (msg "Todo").

Definition table_init (cs : compiler_state) (tidx : tuint) (eidx : tuint)
  : res (list Clight.statement * compiler_state) :=
  Error (msg "Todo").

Definition elem_drop (cs : compiler_state) (idx : tuint)
  : res (list Clight.statement * compiler_state) :=
  Error (msg "Todo").