From Corelib Require Extraction.
Set Extraction Output Directory "compiler".
Extraction Language OCaml.

From Wasm2c Require Import Compiler.

Extraction "compiler.ml" compile.
