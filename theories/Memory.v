From Wasm Require Import datatypes datatypes_properties operations numerics.
From Stdlib Require Import PArith NArith ZArith String List.
From compcert Require cfrontend.Clight cfrontend.Ctypes cfrontend.Cop common.AST common.Errors lib.Integers.
From compcert Require Import export.Ctypesdefs.
From Wasm2c Require Import Ident Util.

Import ListNotations.
Import Errors.

Local Open Scope error_monad_scope.

Definition mem_struct_id : AST.ident := ident_of_struct 0.
Definition tmem : Ctypes.type := Ctypes.Tstruct mem_struct_id Ctypes.noattr.
Definition tmem_ptr : Ctypes.type := tptr tmem.

Definition memfield_data      : AST.ident := ident_of_mem_field 0.
Definition memfield_data_end  : AST.ident := ident_of_mem_field 1.
Definition memfield_pages     : AST.ident := ident_of_mem_field 2.
Definition memfield_min_pages : AST.ident := ident_of_mem_field 3.
Definition memfield_max_pages : AST.ident := ident_of_mem_field 4.
Definition memfield_size      : AST.ident := ident_of_mem_field 5.

Definition mem_composite : Ctypes.composite_definition :=
  Ctypes.Composite mem_struct_id Ctypes.Struct [
    Ctypes.Member_plain memfield_data      (tptr tuchar);
    Ctypes.Member_plain memfield_data_end  (tptr tuchar);
    Ctypes.Member_plain memfield_pages     tulong;
    Ctypes.Member_plain memfield_min_pages tulong;
    Ctypes.Member_plain memfield_max_pages tulong;
    Ctypes.Member_plain memfield_size      tulong
  ] Ctypes.noattr.


(* imported memory; nil means the memory is only declared, not defined *)
Definition mem_extern : AST.globvar Ctypes.type :=
  AST.mkglobvar tmem nil false false.

(* defined memory *)
Definition mem_def (ce : Ctypes.composite_env) : AST.globvar Ctypes.type :=
  AST.mkglobvar tmem [AST.Init_space (Ctypes.sizeof ce tmem)] false false.

Definition mem_var : Clight.expr := Clight.Evar (ident_of_mem 0) tmem.
Definition mem_field (f : AST.ident) (ty : Ctypes.type) 
  : Clight.expr := Clight.Efield (mem_var) f ty.
Definition mem_data : Clight.expr := 
  mem_field memfield_data (tptr tuchar).
Definition mem_size : Clight.expr := mem_field memfield_size tulong.

Definition wasm_page_size : Z := (Z.of_nat 65536).
Definition wasm_max_pages : N := 65536%N.

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



Definition set_mem_field (f : AST.ident) (ty : Ctypes.type) (e : Clight.expr)
  : Clight.statement
  := Clight.Sassign (mem_field f ty) e.


Definition alloc_mem_stmts (min max : N) : list Clight.statement :=
  let bytes := (Z.of_N min * wasm_page_size)%Z in
  [ set_mem_field memfield_min_pages tulong (const_u64 (Z.of_N min));
    set_mem_field memfield_max_pages tulong (const_u64 (Z.of_N max));
    set_mem_field memfield_pages     tulong (const_u64 (Z.of_N min));
    set_mem_field memfield_size      tulong (const_u64 bytes);
    Clight.Scall (Some (ident_of_local 0))
      (Clight.Evar ident_calloc tcalloc)
      [const_u64 bytes; const_u64 1];
    (* check return value of calloc *)
    set_mem_field memfield_data (tptr tuchar)
      (Clight.Ecast (Clight.Etempvar (ident_of_local 0) (tptr tvoid)) (tptr tuchar));
    set_mem_field memfield_data_end (tptr tuchar)
      (Clight.Ebinop Cop.Oadd mem_data (const_u64 bytes) (tptr tuchar))
  ].

(** extract min and max pages from Wasm memory *)
Definition limits_of_mem (mem : module_mem) : N * N :=
  let lim := mem.(modmem_type) in
    (lim.(lim_min), match lim.(lim_max) with Some x => x | None => wasm_max_pages end).


Definition data_globvar (bs : list byte) : AST.globvar Ctypes.type :=
  AST.mkglobvar
    (tarray tuchar (Z.of_nat (List.length bs)))
    (List.map (fun b => AST.Init_int8 (Integers.Int.repr (wasmcompcert.lib.Integers.Byte.unsigned b))) bs)
    true false.

(**  *)
Definition const_offset (e : expr) : res Z :=
  match e with
  | [BI_const_num (VAL_int32 k)] => OK (Wasm_int.Z_of_uint i32m k)
  | _ => Error (msg "data segment offset must be a constant i32")
  end.

(** returns a Clight statement to copy Z bytes of data from src to dst *)
Definition copy_data (dst src : Clight.expr) (len : Z) : Clight.statement :=
  Clight.Sbuiltin None (AST.EF_memcpy len 1) [tptr tvoid; tptr tvoid] [dst; src].

(** compile data segments into a list of global variables and the statements to 
    instantiate them *)
Fixpoint compile_datas (idx : N) (ds : list module_data)
  : res (list (AST.ident * AST.globdef Clight.fundef Ctypes.type)
         * list Clight.statement) :=
  match ds with
  | nil => OK (nil, nil)
  | dat :: rest =>
    match dat.(moddata_mode) with
    | MD_passive => Error (msg "passive data segments not supported")
    | MD_active midx ofs =>
      if negb (N.eqb midx 0) then
        Error (msg "non-zero memory index not supported")
      else
        do off <- const_offset ofs;
        let bs    := dat.(moddata_init) in
        let len   := Z.of_nat (List.length bs) in
        let tdata := tarray tuchar len in
        let id    := ident_of_data idx in 
        let src   := Clight.Ecast
                      (Clight.Eaddrof (Clight.Evar id tdata) (tptr tdata))
                      (tptr tvoid) in
        let dst   := Clight.Ecast
                      (Clight.Ebinop Cop.Oadd mem_data (const_u64 off) (tptr tuchar))
                      (tptr tvoid) in
        do (gvs, stmts) <- compile_datas (N.succ idx) rest;
        OK ((id, AST.Gvar (data_globvar bs)) :: gvs,
            copy_data dst src len :: stmts)
    end
  end.

(** construct the memory instantiation function *)
Definition compile_instantiate (m : module)
  : res (list (AST.ident * AST.globdef Clight.fundef Ctypes.type)) :=
  do alloc <- match m.(mod_mems) with
              | nil      => OK nil
              | mem :: _ => let (mn, mx) := limits_of_mem mem in
                            OK (alloc_mem_stmts mn mx)
              end;
  do (data_defs, data_stmts) <- compile_datas 0 m.(mod_datas);
  let f := Clight.mkfunction tvoid AST.cc_default nil nil
           [(ident_of_local 0, tptr tvoid)]
           (seq_of_list (alloc ++ data_stmts ++ [Clight.Sreturn None])) in
  OK (data_defs ++ [(ident_instantiate, AST.Gfun (Ctypes.Internal f))]).

(** compile a list of Wasm imported memories into a list of Clight global 
    variables *)
Fixpoint compile_mem_import (imps : list module_import)
  : res (list (AST.ident * AST.globdef Clight.fundef Ctypes.type)) :=
  match imps with
  | nil => OK nil
  | mem :: rest =>
    match mem.(imp_desc) with
    | MID_mem mem_ty => OK [(ident_of_mem 0, AST.Gvar mem_extern)]
    | _ => compile_mem_import rest
    end
  end.

(** compile Wasm memories into Clight memories *)
Definition compile_mem (m : module) (ce : Ctypes.composite_env)
  : res (list (AST.ident * AST.globdef Clight.fundef Ctypes.type)) :=
  if N.ltb 1 (n_imported_memories m + n_defined_memories m) then
    Error (msg "only one memory allowed in Wasm 1.0")
  else
    match m.(mod_mems) with
    | nil => compile_mem_import m.(mod_imports)
    | mem :: _ => 
      OK ([(ident_of_mem 0, AST.Gvar (mem_def ce))]) 
    end
  .
