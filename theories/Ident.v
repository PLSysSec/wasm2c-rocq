From Stdlib Require Import PArith NArith.
From compcert Require common.AST cfrontend.Clight cfrontend.Ctypes common.AST.
From compcert Require Import export.Ctypesdefs.

(* tags to partition variable identifiers *)
Definition ident_of_func            (i : N) : AST.ident := ((N.succ_pos i)~0~0~0~0~0)%positive.
Definition ident_of_global          (i : N) : AST.ident := ((N.succ_pos i)~0~0~0~0~1)%positive.
Definition ident_of_local           (i : N) : AST.ident := ((N.succ_pos i)~0~0~0~1~0)%positive.
Definition ident_of_i32_slot        (i : N) : AST.ident := ((N.succ_pos i)~0~0~0~1~1)%positive.
Definition ident_of_i64_slot        (i : N) : AST.ident := ((N.succ_pos i)~0~0~1~0~0)%positive.
Definition ident_of_f32_slot        (i : N) : AST.ident := ((N.succ_pos i)~0~0~1~0~1)%positive.
Definition ident_of_f64_slot        (i : N) : AST.ident := ((N.succ_pos i)~0~0~1~1~0)%positive.
Definition ident_of_ref_slot        (i : N) : AST.ident := ((N.succ_pos i)~0~0~1~1~1)%positive.
Definition ident_of_struct          (i : N) : AST.ident := ((N.succ_pos i)~0~1~0~0~0)%positive.
Definition ident_of_runtime         (i : N) : AST.ident := ((N.succ_pos i)~0~1~0~0~1)%positive.
Definition ident_of_data            (i : N) : AST.ident := ((N.succ_pos i)~0~1~0~1~0)%positive.
Definition ident_of_elem            (i : N) : AST.ident := ((N.succ_pos i)~0~1~0~1~1)%positive.
Definition ident_of_scratch         (i : N) : AST.ident := ((N.succ_pos i)~0~1~1~0~0)%positive.
Definition ident_of_builtin         (i : N) : AST.ident := ((N.succ_pos i)~0~1~1~0~1)%positive.
Definition ident_of_inst_field      (i : N) : AST.ident := ((N.succ_pos i)~0~1~1~1~0)%positive.
Definition ident_of_mem_field       (i : N) : AST.ident := ((N.succ_pos i)~0~1~1~1~1)%positive.
Definition ident_of_elem_field     (i : N) : AST.ident := ((N.succ_pos i)~1~0~0~0~0)%positive.
Definition ident_of_table_field    (i : N) : AST.ident := ((N.succ_pos i)~1~0~0~0~1)%positive.

Definition inst_struct_id   : AST.ident := ident_of_struct 0.
Definition tinst            : Ctypes.type := Ctypes.Tstruct inst_struct_id Ctypes.noattr.
Definition tinst_ptr        : Ctypes.type := tptr tinst.

Definition mem_struct_id    : AST.ident := ident_of_struct 1.
Definition tmem             : Ctypes.type := Ctypes.Tstruct mem_struct_id Ctypes.noattr.
Definition tmem_ptr         : Ctypes.type := tptr tmem.

Definition elem_struct_id   : AST.ident := ident_of_struct 2.
Definition t_elem           : Ctypes.type := Ctypes.Tstruct elem_struct_id Ctypes.noattr.

Definition table_struct_id  : AST.ident := ident_of_struct 3.
Definition ttable           : Ctypes.type := Ctypes.Tstruct table_struct_id Ctypes.noattr.
Definition ttable_ptr       : Ctypes.type := tptr ttable.

Definition mem_data      : AST.ident := ident_of_mem_field 0.
Definition mem_pages     : AST.ident := ident_of_mem_field 1.
Definition mem_min_pages : AST.ident := ident_of_mem_field 2.
Definition mem_max_pages : AST.ident := ident_of_mem_field 3.
Definition mem_size      : AST.ident := ident_of_mem_field 4.

Definition elem_type   : AST.ident := ident_of_elem_field 0.
Definition elem_ptr    : AST.ident := ident_of_elem_field 1.
Definition elem_inst   : AST.ident := ident_of_elem_field 2.

Definition table_data   : AST.ident := ident_of_table_field 0.
Definition table_size   : AST.ident := ident_of_table_field 1.
Definition table_min    : AST.ident := ident_of_table_field 2.
Definition table_max    : AST.ident := ident_of_table_field 3.

Definition inst_mem      : AST.ident := ident_of_inst_field 0.
Definition inst_trapflag : AST.ident := ident_of_inst_field 1.
Definition inst_globals  : AST.ident := ident_of_inst_field 2.
Definition inst_tables   : AST.ident := ident_of_inst_field 3.

Definition ident_instantiate : AST.ident := ident_of_builtin 0.

Definition ident_calloc      : AST.ident := ident_of_runtime 0.
Definition ident_trap        : AST.ident := ident_of_runtime 1.
Definition ident_realloc     : AST.ident := ident_of_runtime 2.
Definition ident_memset      : AST.ident := ident_of_runtime 3.

Definition ident_inst        : AST.ident := (1~1~0~0~0)%positive.
