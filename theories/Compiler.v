From Wasm Require Import datatypes.
From Stdlib Require Import PArith.
From compcert Require cfrontend.Clight cfrontend.Ctypes common.AST common.Errors.

(*
    - module defined in WasmCert-Coq/theories/datatypes.v:740
    - Clight.program defined in CompCert/cfrontend/Ctypes.v:1545 (res discharges
      a proof obligation)
*)
Definition compile (m : module) : Errors.res Clight.program.
Admitted.
    (* shape of wasm2c compile is

        write_header(module)
        write_source(module)
        return
    
    *)

(* compile functions and global variables *)
Definition compile_funcs_globals (funcs : list module_func) : list (AST.ident * AST.globdef Clight.fundef Ctypes.type).
Admitted.