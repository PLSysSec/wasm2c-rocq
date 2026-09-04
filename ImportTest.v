  (** Verifies that all three namespaces resolve and that the two CompCert
      roots are simultaneously loadable. *)

  From compcert     Require lib.Integers lib.Floats common.Memdata.
  From wasmcompcert Require lib.Integers common.Memdata.
  From Wasm         Require datatypes operations.

  (* 1. Real CompCert *)
  Check compcert.lib.Integers.Int.repr.
  Check compcert.lib.Floats.Float.add.
  Check compcert.common.Memdata.encode_int.

  (* 2. WasmCert's subset *)
  Check wasmcompcert.lib.Integers.Int.repr.
  Check wasmcompcert.common.Memdata.encode_int.

  (* 3. WasmCert proper. *)
  Check Wasm.datatypes.u32.
  Check Wasm.operations.v128_size.