From Wasm Require Import datatypes.
From Stdlib Require Import PArith.
From compcert Require cfrontend.Clight cfrontend.Ctypes common.Errors.

(**
    - module defined in WasmCert-Coq/theories/datatypes.v:740
    - Clight.program defined in CompCert/cfrontend/Ctypes.v:1545 (res discharges
      a proof obligation)
*)
Definition compile (m : module) : Errors.res Clight.program :=
    Ctypes.make_program nil nil nil 1%positive.