FROM ocaml/opam:ubuntu-22.04-ocaml-5.4

# Install system packages as root
USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
        libgmp-dev \
        m4 \
        pkg-config \
        git \
        make \
        ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Then switch back to opam user (installing as root is weird)
USER opam
WORKDIR /home/opam/wasm2c-rocq

# Register rocq package archive
RUN opam repo add coq-released https://coq.inria.fr/opam/released && opam update

# WasmCert-Coq needs rocq 9.0 or 9.1
# CompCert 3.18 needs rocq 9.0, 9.1, or 9.2
# So use the WasmCert .opam file to specify rocq install
COPY --chown=opam:opam WasmCert-Coq/coq-wasm.opam WasmCert-Coq/coq-wasm.opam
RUN opam install -y coq-flocq.4.2.2 && \
    opam install -y --deps-only ./WasmCert-Coq/coq-wasm.opam

COPY --chown=opam:opam WasmCert-Coq WasmCert-Coq

# WasmCert has a version of CompCert as part of its repo. This creates path 
# collisions -- does `Require compcert` import CompCert, or the WasmCert version
# of CompCert? Instead, we just change the name of WasmCert's compcert package 
# to wasmcompcert
RUN cd WasmCert-Coq && \
    grep -rl '^From compcert' theories compcert \
    | xargs sed -i 's/^From compcert Require/From wasmcompcert Require/' && \
    sed -i 's/(name compcert)/(name wasmcompcert)/' compcert/dune && \
    sed -i 's/\bcompcert\b/wasmcompcert/' theories/dune src/dune && \
    sed -i 's|^-R _build/default/compcert compcert$|-R _build/default/compcert wasmcompcert|' _CoqProject && \
    ! grep -rn '^From compcert\|(theories.*\bcompcert\b\|(name compcert)' theories compcert src

# Build WasmCert proofs
ARG JOBS=4
RUN opam install -y -j "${JOBS}" ./WasmCert-Coq

COPY --chown=opam:opam CompCert CompCert

# Install CompCert (note: CompCert target arch pinned at x86_64)
ARG CC_TARGET=x86_64-linux
RUN cd CompCert && \
    opam exec -- ./configure -clightgen -install-rocqdev -use-external-Flocq -no-runtime-lib \
        -prefix /home/opam/.local "${CC_TARGET}"

# Build CompCert proofs
RUN cd CompCert && \
    opam exec -- make -j"${JOBS}" && \
    opam exec -- make install

COPY --chown=opam:opam _CoqProject .

# Add vsrocq language server
RUN opam install -y vsrocq-language-server.2.4.3+1 && \
      ln -sf "$(opam var bin)/vsrocqtop" /home/opam/.local/bin/vsrocqtop

ENV OPAMYES=1
ENTRYPOINT ["opam", "exec", "--"]
CMD ["bash"]