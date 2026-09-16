From Stdlib Require Import PArith NArith.
From compcert Require common.AST.

(* tags to partition variable identifiers *)
Definition ident_of_func       (i : N) : AST.ident := ((N.succ_pos i)~0~0~0~0)%positive.
Definition ident_of_global     (i : N) : AST.ident := ((N.succ_pos i)~0~0~0~1)%positive.
Definition ident_of_local      (i : N) : AST.ident := ((N.succ_pos i)~0~0~1~0)%positive.
Definition ident_of_i32_slot   (i : N) : AST.ident := ((N.succ_pos i)~0~0~1~1)%positive.
Definition ident_of_i64_slot   (i : N) : AST.ident := ((N.succ_pos i)~0~1~0~0)%positive.
Definition ident_of_f32_slot   (i : N) : AST.ident := ((N.succ_pos i)~0~1~0~1)%positive.
Definition ident_of_f64_slot   (i : N) : AST.ident := ((N.succ_pos i)~0~1~1~0)%positive.
Definition ident_of_ref_slot   (i : N) : AST.ident := ((N.succ_pos i)~0~1~1~1)%positive.
Definition ident_of_inst_field (i : N) : AST.ident := ((N.succ_pos i)~1~0~0~1)%positive.
Definition ident_of_struct     (i : N) : AST.ident := ((N.succ_pos i)~1~0~1~0)%positive.
Definition ident_of_runtime    (i : N) : AST.ident := ((N.succ_pos i)~1~0~1~1)%positive.
Definition ident_of_data       (i : N) : AST.ident := ((N.succ_pos i)~1~1~0~0)%positive.
Definition ident_of_scratch    (i : N) : AST.ident := ((N.succ_pos i)~1~1~0~1)%positive.
Definition ident_of_builtin    (i : N) : AST.ident := ((N.succ_pos i)~1~1~1~0)%positive.

Definition ident_calloc      : AST.ident := ident_of_runtime 0.
Definition ident_instantiate : AST.ident := ident_of_builtin 0.
Definition ident_trap        : AST.ident := ident_of_runtime 1.
Definition ident_inst        : AST.ident := (1~1~0~0~0)%positive.
