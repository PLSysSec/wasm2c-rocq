From Wasm Require Import datatypes datatypes_properties operations numerics.
From Stdlib Require Import PArith NArith ZArith String List.
From compcert Require cfrontend.Clight cfrontend.Ctypes cfrontend.Cop common.AST common.Errors lib.Integers.
From compcert Require Import export.Ctypesdefs.
From Wasm2c Require Import Ident Util Extern.

Import ListNotations.
Import Errors.

Local Open Scope error_monad_scope.

Definition mem_composite : Ctypes.composite_definition :=
  Ctypes.Composite mem_struct_id Ctypes.Struct [
    Ctypes.Member_plain mem_data      (tptr tuchar);
    Ctypes.Member_plain mem_pages     tulong;
    Ctypes.Member_plain mem_min_pages tulong;
    Ctypes.Member_plain mem_max_pages tulong;
    Ctypes.Member_plain mem_size      tulong
  ] Ctypes.noattr.

Definition elem_composite : Ctypes.composite_definition :=
  Ctypes.Composite elem_struct_id Ctypes.Struct [
    Ctypes.Member_plain elem_type   tulong;
    Ctypes.Member_plain elem_ptr    (tptr tvoid);
    Ctypes.Member_plain elem_inst   tulong
  ] Ctypes.noattr.

Definition table_composite : Ctypes.composite_definition :=
  Ctypes.Composite table_struct_id Ctypes.Struct [
    Ctypes.Member_plain table_data  (tptr t_elem);
    Ctypes.Member_plain table_size  tulong;
    Ctypes.Member_plain table_min   tulong;
    Ctypes.Member_plain table_max   tulong
  ] Ctypes.noattr.

Definition inst_composite : Ctypes.composite_definition :=
  Ctypes.Composite inst_struct_id Ctypes.Struct [
    Ctypes.Member_plain inst_mem      tmem_ptr;
    Ctypes.Member_plain inst_trapflag tuint;
    Ctypes.Member_plain inst_globals  (tptr (tptr tvoid));
    Ctypes.Member_plain inst_tables   ttable_ptr
  ] Ctypes.noattr.

(** pointer reference to the instance *)
Definition inst_ptr : Clight.expr := Clight.Etempvar ident_inst tinst_ptr.

Definition inst_members : Ctypes.members :=
  match inst_composite with Ctypes.Composite _ _ ms _ => ms end.

Definition mem_members : Ctypes.members :=
  match mem_composite with Ctypes.Composite _ _ ms _ => ms end.

Definition elem_members : Ctypes.members :=
  match elem_composite with Ctypes.Composite _ _ ms _ => ms end.

Definition table_members : Ctypes.members :=
  match table_composite with Ctypes.Composite _ _ ms _ => ms end.

(** get a reference to an instance field *)
Definition inst_field (f : AST.ident) : Clight.expr :=
  match Ctypes.field_type f inst_members with
  | OK ty => Clight.Efield (Clight.Ederef inst_ptr tinst) f ty
  | Error _ => Clight.Efield (Clight.Ederef inst_ptr tinst) f tvoid
  end.

(** get a reference to a memory field *)
Definition mem_field (f : AST.ident) : Clight.expr :=
  match Ctypes.field_type f mem_members with
  | OK ty => Clight.Efield (Clight.Ederef (inst_field inst_mem) tmem) f ty
  | Error _ => Clight.Efield (Clight.Ederef (inst_field inst_mem) tmem) f tvoid
  end.

(** get a reference to a table field *)
Definition table_field (f : AST.ident) : Clight.expr :=
  match Ctypes.field_type f table_members with
  | OK ty => Clight.Efield (Clight.Ederef (inst_field inst_tables) ttable) f ty
  | Error _ => Clight.Efield (Clight.Ederef (inst_field inst_tables) ttable) f tvoid
  end.

(** get a reference to a elem field *)
Definition elem_field (f : AST.ident) : Clight.expr :=
  match Ctypes.field_type f elem_members with
  | OK ty => Clight.Efield (Clight.Ederef (table_field table_data) tmem) f ty
  | Error _ => Clight.Efield (Clight.Ederef (table_field table_data) tmem) f tvoid
  end.

(** set instance field to a value *)
Definition set_inst_field (f : AST.ident) (e : Clight.expr)
  : res Clight.statement :=
  OK (Clight.Sassign (inst_field f) e).

(** set memory field to a value *)
Definition set_mem_field (f : AST.ident) (e : Clight.expr)
  : res Clight.statement :=
  OK (Clight.Sassign (mem_field f) e).

(** set table field to a value *)
Definition set_table_field (f : AST.ident) (e : Clight.expr)
  : res Clight.statement :=
  OK (Clight.Sassign (table_field f) e).

(** set element field to a value *)
Definition set_elem_field (f : AST.ident) (e : Clight.expr)
  : res Clight.statement :=
  OK (Clight.Sassign (elem_field f) e).

Definition wasm_page_size : N := 65536%N.
Definition wasm_max_pages : N := 65536%N.
Definition wasm_max_table_size : N := 4294967295%N. (* u32 max *)

(** extract min and max from a limits record, defaulting the max when absent *)
Definition bounds_of_limits (default_max : N) (lim : limits) : N * N :=
  (lim.(lim_min), match lim.(lim_max) with Some x => x | None => default_max end).

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

Definition alloc_def_table_stmts (min max : N) : res (list Clight.statement) :=
  let table_tmp := ident_of_local 2 in
  let elem_tmp  := ident_of_local 3 in
  (* allocate table struct at table_tmp *)
  let i0 := Clight.Scall (Some table_tmp)
              (Clight.Evar ident_calloc tcalloc)
              [const_u64 1; Clight.Esizeof ttable tulong] in
  let table_ptr :=
    Clight.Ecast (Clight.Etempvar table_tmp (tptr tvoid)) ttable_ptr in
  do i1 <- set_inst_field inst_tables table_ptr;
  do i2 <- set_table_field table_size (const_u64 min);
  do i3 <- set_table_field table_min  (const_u64 min);
  do i4 <- set_table_field table_max  (const_u64 max);
  (* allocate space for elements to be set at elem_tmp *)
  let i5 := Clight.Scall (Some elem_tmp)
            (Clight.Evar ident_calloc tcalloc)
            [const_u64 min; Clight.Esizeof t_elem tulong] in
  let elements_ptr := Clight.Ecast
    (Clight.Etempvar elem_tmp (tptr tvoid)) (tptr t_elem) in
  do i6 <- set_table_field table_data elements_ptr;
  OK [i0; i1; i2; i3; i4; i5; i6].

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
  | _ => Error (msg "segment offset must be a constant i32")
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
               (mem_field mem_data)
               (const_u64 (Z.to_N off))
               (tptr tuchar))
            (tptr tvoid) in
        do (gvs, stmts) <- compile_datas (N.succ idx) rest;
        OK ((id, AST.Gvar (data_globvar bs)) :: gvs,
            copy_data dst src len :: stmts)
    end
  end.

(** turn one constant element-init expression into the funcidx it refers to *)
Definition const_elem_ref (elem_ty : reference_type) (e : expr) : res (option N) :=
  match elem_ty with
  | T_funcref =>
    match e with
    | [BI_ref_func idx] => OK (Some idx)
    | [BI_ref_null _]   => OK None
    | _ => Error (msg "unsupported constant element expression")
    end
  | T_externref => Error (msg "external ref elements not supported")
  end.

(** turn a whole element segment's init list into the funcidx that 
    should land in each successive table slot *)
Fixpoint const_elem_refs (elem_ty : reference_type) (exprs : list expr)
  : res (list (option N)) :=
  match exprs with
  | nil => OK nil
  | e :: rest =>
    do r     <- const_elem_ref elem_ty e;
    do rest' <- const_elem_refs elem_ty rest;
    OK (r :: rest')
  end.

(** statements to populate one table slot from the funcidx it should hold *)
Definition elem_slot_stmts (m : module) (slot_ptr : Clight.expr) (fidx : option N)
  : res (list Clight.statement) :=
  match fidx with
  | None =>
    (* should we actually return a null pointer? *)
    OK [
      Clight.Sassign (Clight.Efield (Clight.Ederef slot_ptr t_elem) elem_type tulong) (const_u64 0);
      Clight.Sassign (Clight.Efield (Clight.Ederef slot_ptr t_elem) elem_ptr (tptr tvoid))
        (Clight.Econst_int Integers.Int.zero (tptr tvoid));
      Clight.Sassign (Clight.Efield (Clight.Ederef slot_ptr t_elem) elem_inst tulong) (const_u64 0)
    ]
  | Some idx =>
    match lookup_N m.(mod_funcs) idx with
    | None => Error (msg "function idx not found in module")
    | Some f =>
      match lookup_N m.(mod_types) f.(modfunc_type) with
      | None => Error (msg "type idx not found in module")
      | Some func_type =>
        do tag     <- signature_tag func_type;
        do func_ty <- func_ptr_type func_type;
        let func_addr :=
          Clight.Ecast
            (Clight.Eaddrof (Clight.Evar (ident_of_func idx) func_ty) (tptr func_ty))
            (tptr tvoid) in
        OK [
          Clight.Sassign (Clight.Efield (Clight.Ederef slot_ptr t_elem) elem_type tulong) (const_u64 tag);
          Clight.Sassign (Clight.Efield (Clight.Ederef slot_ptr t_elem) elem_ptr (tptr tvoid)) func_addr;
          Clight.Sassign (Clight.Efield (Clight.Ederef slot_ptr t_elem) elem_inst tulong)
            (Clight.Ecast inst_ptr tulong)
        ]
      end
    end
  end.

(** statements to populate consecutive table slots starting at [base_off],
    one per entry of [refs] *)
Fixpoint elem_slots_stmts (m : module) (table_data_ptr : Clight.expr) (base_off : N)
                           (refs : list (option N))
  : res (list Clight.statement) :=
  match refs with
  | nil => OK nil
  | r :: rest =>
    let slot_ptr := Clight.Ebinop Cop.Oadd table_data_ptr (const_u64 base_off) (tptr t_elem) in
    do stmts  <- elem_slot_stmts m slot_ptr r;
    do stmts' <- elem_slots_stmts m table_data_ptr (N.succ base_off) rest;
    OK (stmts ++ stmts')
  end.

(** compile active element segments into the statements that populate tables *)
Fixpoint compile_elems (m : module) (es : list module_element)
  : res (list Clight.statement) :=
  match es with
  | nil => OK nil
  | elem :: rest =>
    do stmts <-
      (match elem.(modelem_mode) with
       | ME_passive | ME_declarative => OK nil
       | ME_active tidx offs =>
          match lookup_N m.(mod_tables) tidx with
          | None => Error (msg "table idx oob")
          | Some table =>
            if negb (reference_type_eqb elem.(modelem_type) table.(modtab_type).(tt_elem_type)) then
              Error (msg "element segment type does not match table's element type")
            else
              do off  <- const_segment_offset offs;
              do refs <- const_elem_refs elem.(modelem_type) elem.(modelem_init);
              let n_entries := N.of_nat (List.length refs) in
              let (mn, _) := bounds_of_limits wasm_max_table_size table.(modtab_type).(tt_limits) in
              if N.ltb mn (Z.to_N off + n_entries)%N then
                Error (msg "element segment overruns table")
              else
                let table_data_ptr :=
                  Clight.Efield (Clight.Ederef (inst_field inst_tables) ttable) table_data (tptr t_elem) in
                elem_slots_stmts m table_data_ptr (Z.to_N off) refs
          end
       end);
    do stmts' <- compile_elems m rest;
    OK (stmts ++ stmts')
  end.

Definition init_trapflag_statements : res (list Clight.statement) :=
  let zero := Clight.Econst_int Integers.Int.zero tuint in
  do i0 <- set_inst_field inst_trapflag zero;
  OK [ i0 ].

(** construct the instance instantiation function *)
Definition compile_instantiate (m : module)
  : res (list (AST.ident * AST.globdef Clight.fundef Ctypes.type)) :=
  (* Only support one memory and one table for now? *)
  do mem_alloc <- match m.(mod_mems) with
                  | nil      => OK nil
                  | mem :: _ => let (mn, mx) := (bounds_of_limits wasm_max_pages mem.(modmem_type)) in
                                alloc_def_mem_stmts mn mx
                  end;
  do table_alloc <- match m.(mod_tables) with
                    | nil   => OK nil
                    | table :: _ => let (mn, mx) := (bounds_of_limits wasm_max_table_size table.(modtab_type).(tt_limits)) in
                                    alloc_def_table_stmts mn mx
                    end;
  do trapflag_init <- init_trapflag_statements;
  do (data_defs, data_stmts) <- compile_datas 0 m.(mod_datas);
  do elem_stmts <- compile_elems m m.(mod_elems);
  let body := seq_of_list (
    trapflag_init ++
    mem_alloc ++
    table_alloc ++
    data_stmts ++
    elem_stmts ++
    [Clight.Sreturn None]
  ) in
  let f := Clight.mkfunction
             tvoid AST.cc_default
             [(ident_inst, tinst_ptr)]        (* params *)
             nil                              (* vars   *)
             [(ident_of_local 0, tptr tvoid); (* temps  *)
              (ident_of_local 1, tptr tvoid)]
             body in
  OK (data_defs ++ [(ident_instantiate, AST.Gfun (Ctypes.Internal f))]).
