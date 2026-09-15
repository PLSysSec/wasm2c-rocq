From Wasm Require Import datatypes datatypes_properties operations numerics.
From Stdlib Require Import PArith NArith ZArith String List.
From compcert Require cfrontend.Clight cfrontend.Ctypes cfrontend.Cop common.AST common.Errors lib.Integers.
From compcert Require Import export.Ctypesdefs.
From Wasm2c Require Import Ident Util.

Import ListNotations.
Import Errors.

Local Open Scope error_monad_scope.

Definition inst_struct_id : AST.ident := ident_of_struct 0.
Definition tinst : Ctypes.type := Ctypes.Tstruct inst_struct_id Ctypes.noattr.
Definition tinst_ptr : Ctypes.type := tptr tinst.

Definition mem_struct_id : AST.ident := ident_of_struct 1.
Definition tmem : Ctypes.type := Ctypes.Tstruct mem_struct_id Ctypes.noattr.
Definition tmem_ptr : Ctypes.type := tptr tmem.

Definition mem_data      : AST.ident := ident_of_inst_field 0.
Definition mem_pages     : AST.ident := ident_of_inst_field 1.
Definition mem_min_pages : AST.ident := ident_of_inst_field 2.
Definition mem_max_pages : AST.ident := ident_of_inst_field 3.
Definition mem_size      : AST.ident := ident_of_inst_field 4.

Definition inst_mem      : AST.ident := ident_of_inst_field 5.
Definition inst_trapflag : AST.ident := ident_of_inst_field 6.
Definition inst_globals  : AST.ident := ident_of_inst_field 7.

Definition mem_composite : Ctypes.composite_definition :=
  Ctypes.Composite mem_struct_id Ctypes.Struct [
    Ctypes.Member_plain mem_data      (tptr tuchar);
    Ctypes.Member_plain mem_pages     tulong;
    Ctypes.Member_plain mem_min_pages tulong;
    Ctypes.Member_plain mem_max_pages tulong;
    Ctypes.Member_plain mem_size      tulong
  ] Ctypes.noattr.

Definition inst_composite : Ctypes.composite_definition :=
  Ctypes.Composite inst_struct_id Ctypes.Struct [
    Ctypes.Member_plain inst_mem      tmem_ptr; 
    Ctypes.Member_plain inst_trapflag (tptr tuint);
    Ctypes.Member_plain inst_globals  (tptr (tptr tvoid))
  ] Ctypes.noattr.

(** look up type of memory field*)
Definition mem_field_type (f : AST.ident) : res Ctypes.type :=
  if Pos.eqb f mem_data      then OK (tptr tuchar) else
  if Pos.eqb f mem_pages     then OK tulong        else
  if Pos.eqb f mem_min_pages then OK tulong        else
  if Pos.eqb f mem_max_pages then OK tulong        else
  if Pos.eqb f mem_size      then OK tulong        else
  Error (msg "invalid memory field").

(** look up type of instance field *)
Definition inst_field_type (f : AST.ident) : res Ctypes.type :=
  if Pos.eqb f inst_mem       then OK tmem_ptr            else 
  if Pos.eqb f inst_trapflag  then OK (tptr tuint)        else
  if Pos.eqb f inst_globals   then OK (tptr (tptr tvoid)) else
  Error (msg "invalid instance field").

(** pointer reference to the instance *)
Definition inst_ptr : Clight.expr := Clight.Etempvar ident_inst tinst_ptr.

(** get a reference to an instance field *)
Definition inst_field (f : AST.ident) (ty : Ctypes.type) : Clight.expr :=
  Clight.Efield (Clight.Ederef inst_ptr tinst) f ty.

(** get a reference to a memory field *)
Definition mem_field (f : AST.ident) (ty : Ctypes.type) : Clight.expr :=
  Clight.Efield (Clight.Ederef (inst_field inst_mem tmem_ptr) tmem) f ty.

(** set instance field to a value *)
Definition set_inst_field (f : AST.ident) (e : Clight.expr) 
  : res Clight.statement :=
  do ty <- inst_field_type f;
  OK (Clight.Sassign (inst_field f ty) e).

(** set memory field to a value *)
Definition set_mem_field (f : AST.ident) (e : Clight.expr)
  : res Clight.statement :=
  do ty <- mem_field_type f;
  OK (Clight.Sassign (mem_field f ty) e).

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

Definition wasm_page_size : N := 65536%N.
Definition wasm_max_pages : N := 65536%N.

Definition alloc_def_mem_stmts (min max : N) : res (list Clight.statement) :=
  let num_bytes := (min * wasm_page_size)%N in
  let mem_tmp   := ident_of_local 0 in
  let data_tmp  := ident_of_local 1 in
  (* TODO: check return value of calloc *)
  (* local0 = calloc(1, sizeof(struct wasm_memory)) *)
  let i0 := Clight.Scall (Some mem_tmp)
              (Clight.Evar ident_calloc tcalloc)
              [const_u64 1; Clight.Esizeof tmem tulong] in
  let mem_cast := 
    Clight.Ecast (Clight.Etempvar mem_tmp (tptr tvoid)) tmem_ptr in
  (* inst->mem = (struct wasm_memory* ) local0 *)
  do i1 <- set_inst_field inst_mem mem_cast;
  do i2 <- set_mem_field mem_min_pages (const_u64 min);
  do i3 <- set_mem_field mem_max_pages (const_u64 max);
  do i4 <- set_mem_field mem_pages     (const_u64 min);
  do i5 <- set_mem_field mem_size      (const_u64 num_bytes);
  (* TODO: check return value of calloc *)
  (* local1 = calloc(num_bytes, 1) *)
  let i6 := Clight.Scall (Some data_tmp)
              (Clight.Evar ident_calloc tcalloc)
              [const_u64 num_bytes; const_u64 1] in
  let data_cast := Clight.Ecast
    (Clight.Etempvar data_tmp (tptr tvoid)) (tptr tuchar) in
  do i7 <- set_mem_field mem_data data_cast;
  OK [i0; i1; i2; i3; i4; i5; i6; i7].

(* Definition alloc_imp_mem_stmts (min max : N) : res (list Clight.statement).
Admitted. *)

(** returns a Clight statement to copy Z bytes of data from src to dst *)
Definition copy_data (dst src : Clight.expr) (len : Z) : Clight.statement :=
  Clight.Sbuiltin None (AST.EF_memcpy len 1) [tptr tvoid; tptr tvoid] [dst; src].

(** turn list of Wasm bytes into Clight array *)
Definition data_globvar (bs : list byte) : AST.globvar Ctypes.type :=
  AST.mkglobvar
    (tarray tuchar (Z.of_nat (List.length bs)))
    (List.map (fun b => AST.Init_int8 (Integers.Int.repr (wasmcompcert.lib.Integers.Byte.unsigned b))) bs)
    true false. (* true = readonly *)

(** turn Wasm offset expression into Z *)
Definition const_segment_offset (e : expr) : res Z :=
  match e with
  | [BI_const_num (VAL_int32 k)] => OK (Wasm_int.Z_of_uint i32m k)
  | _ => Error (msg "data segment offset must be a constant i32")
  end.

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
        do off <- const_segment_offset ofs;
        let bs    := dat.(moddata_init) in
        let len   := Z.of_nat (List.length bs) in
        let tdata := tarray tuchar len in
        let id    := ident_of_data idx in 
        let src   := Clight.Ecast
                      (Clight.Eaddrof (Clight.Evar id tdata) (tptr tdata))      
                      (tptr tvoid) in
        (* TODO: need a bounds check here, or datas will be able to write past 
           the end of memory *)
        let dst   :=
          Clight.Ecast
            (Clight.Ebinop Cop.Oadd (* adding directly to pointer seems sus *)
               (mem_field mem_data (tptr tuchar))
               (const_u64 (Z.to_N off))
               (tptr tuchar))
            (tptr tvoid) in
        do (gvs, stmts) <- compile_datas (N.succ idx) rest;
        OK ((id, AST.Gvar (data_globvar bs)) :: gvs,
            copy_data dst src len :: stmts)
    end
  end.

(** extract min and max pages from a Wasm memory *)
Definition limits_of_mem (mem : module_mem) : N * N :=
  let lim := mem.(modmem_type) in
    (lim.(lim_min), match lim.(lim_max) with Some x => x | None => wasm_max_pages end).

(** construct the instance instantiation function *)
Definition compile_instantiate (m : module)
  : res (list (AST.ident * AST.globdef Clight.fundef Ctypes.type)) :=
  do alloc <- match m.(mod_mems) with
              | nil      => OK nil
              | mem :: _ => let (mn, mx) := limits_of_mem mem in
                            alloc_def_mem_stmts mn mx
              end;
  do (data_defs, data_stmts) <- compile_datas 0 m.(mod_datas);
  let body := seq_of_list (alloc ++ data_stmts ++ [Clight.Sreturn None]) in
  let f := Clight.mkfunction
             tvoid AST.cc_default
             [(ident_inst, tinst_ptr)]     (* params *)
             nil                              (* vars   *)
             [(ident_of_local 0, tptr tvoid); (* temps  *)
              (ident_of_local 1, tptr tvoid)]
             body in
  OK (data_defs ++ [(ident_instantiate, AST.Gfun (Ctypes.Internal f))]).
