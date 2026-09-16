From Stdlib Require Import String List.
From compcert Require cfrontend.Clight cfrontend.Ctypes common.AST.
From compcert Require Import export.Ctypesdefs.
From Wasm2c Require Import Ident.

Import ListNotations.

Definition ttrap : Ctypes.type := Ctypes.Tfunction nil tvoid AST.cc_default.
Definition trap_decl : AST.ident * AST.globdef Clight.fundef Ctypes.type :=
  (ident_trap,
   AST.Gfun (Ctypes.External
    (AST.EF_external "wasm_rt_trap"
      (Ctypes.signature_of_type nil tvoid AST.cc_default))
    nil tvoid AST.cc_default)).

Definition trap_stmt : Clight.statement :=
  Clight.Scall None (Clight.Evar ident_trap ttrap) nil.

Definition calloc_args : list Ctypes.type := [tulong; tulong].
Definition calloc_ret : Ctypes.type := tptr tvoid.
Definition tcalloc : Ctypes.type :=
  Ctypes.Tfunction calloc_args calloc_ret AST.cc_default.

(* using calloc instead of built in malloc because it zeroes memory *)
Definition calloc_decl : AST.ident * AST.globdef Clight.fundef Ctypes.type :=
  (ident_calloc,
   AST.Gfun (Ctypes.External
    (AST.EF_external "calloc"
      (Ctypes.signature_of_type calloc_args calloc_ret AST.cc_default))
    calloc_args calloc_ret AST.cc_default)).

Definition realloc_args : list Ctypes.type := [tptr tvoid; tulong].
Definition realloc_ret : Ctypes.type := tptr tvoid.
Definition trealloc : Ctypes.type :=
  Ctypes.Tfunction realloc_args realloc_ret AST.cc_default.

Definition realloc_decl : AST.ident * AST.globdef Clight.fundef Ctypes.type :=
  (ident_realloc,
   AST.Gfun (Ctypes.External
    (AST.EF_external "realloc"
      (Ctypes.signature_of_type realloc_args realloc_ret AST.cc_default))
    realloc_args realloc_ret AST.cc_default)).

Definition memset_args : list Ctypes.type := [tptr tvoid; tint; tulong].
Definition memset_ret : Ctypes.type := tptr tvoid.
Definition tmemset : Ctypes.type :=
  Ctypes.Tfunction memset_args memset_ret AST.cc_default.

Definition memset_decl : AST.ident * AST.globdef Clight.fundef Ctypes.type :=
  (ident_memset,
    AST.Gfun (Ctypes.External
      (AST.EF_external "memset"
        (Ctypes.signature_of_type memset_args memset_ret AST.cc_default))
      memset_args memset_ret AST.cc_default)).
