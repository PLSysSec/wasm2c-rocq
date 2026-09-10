From Corelib Require Extraction.
Set Extraction Output Directory "compiler".
Extraction Language OCaml.

From Stdlib Require Import ExtrOcamlNativeString ExtrOcamlBasic.
From Wasm Require Import binary_format_parser.
From Wasm2c Require Import Compiler.

Extraction "compiler.ml" compile run_parse_module_str.
