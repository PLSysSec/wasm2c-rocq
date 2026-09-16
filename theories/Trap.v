From Stdlib Require Import String.
From compcert Require cfrontend.Clight cfrontend.Ctypes common.AST.
From compcert Require Import export.Ctypesdefs.
From Wasm2c Require Import Ident.

(** runtime trap function *)
Definition ttrap : Ctypes.type := Ctypes.Tfunction nil tvoid AST.cc_default.
Definition trap_decl : AST.ident * AST.globdef Clight.fundef Ctypes.type :=
  (ident_trap,
   AST.Gfun (Ctypes.External
    (AST.EF_external "wasm_rt_trap"
      (Ctypes.signature_of_type nil tvoid AST.cc_default))
    nil tvoid AST.cc_default)).

Definition trap_stmt : Clight.statement :=
  Clight.Scall None (Clight.Evar ident_trap ttrap) nil.
