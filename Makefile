extract: clean
	rm compiler/wasm2c && \
	rocq c -R /home/opam/.local/lib/compcert/coq compcert -R theories Wasm2c theories/Compiler.v && \
	rocq c -R /home/opam/.local/lib/compcert/coq compcert -R theories Wasm2c compiler/Extraction.v && \
	cd compiler && ocamlfind ocamlopt -w -a compiler.mli compiler.ml driver.ml -o wasm2c && cd ..

clean:
	rm -f theories/.*.aux theories/*.glob theories/*.vo theories/*.vok \
	      theories/*.vos compiler/.*.aux compiler/*.glob compiler/*.vo \
		  compiler/*.vok compiler/*.vos compiler/*.cmi compiler/*.cmx \
		  compiler/*.mli compiler/*.o
