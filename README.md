# Wasm to C in Rocq
## Building the compiler
- Make sure you're running stuff in the Docker container; it should have all the necessary dependencies
- `make extract` will build the compiler as `compiler/wasm2c`
## Running the compiler
- Once the compiler has been built, you can run it with `./compiler/wasm2c <file.wasm>`
## File structure
- `theories/` contains the compiler written in Rocq, and proofs about it (coming soon!)
- `compiler/` contains the extracted OCaml code
- `examples/` contains some simple programs in Wasm, C, and WasmCert AST
