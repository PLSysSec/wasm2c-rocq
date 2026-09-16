From Wasm Require Import datatypes datatypes_properties operations numerics.
From Stdlib Require Import PArith NArith ZArith String List.
From compcert Require cfrontend.Clight cfrontend.Ctypes cfrontend.Cop common.AST common.Errors lib.Integers.
From compcert Require Import export.Ctypesdefs.
From Wasm2c Require Import Ident Util Instantiate Stack Trap.

Import ListNotations.
Import Errors.

Local Open Scope error_monad_scope.

Definition compile_mem_instr (instr: basic_instruction) (cs : compiler_state) 
  : res (list Clight.statement) :=
  match instr with
  | BI_load ty opt_ty_sx arg => compile_load ty opt_ty_sx arg
  | BI_load_vec varg marg => Error (msg "load_vec not supported")
  | BI_load_vec_lane vw ma li => Error (msg "load_vec_lane not supported")
  | BI_store nt opt ma => Error (msg "store not supported")
  | BI_store_vec ma => Error (msg "store_vec not supported")
  | BI_store_vec_lane vw ma li => Error (msg "store_vec_lane not supported")
  | BI_memory_size => Error (msg "memory_size not supported")
  | BI_memory_grow => Error (msg "memory_grow not supported")
  | BI_memory_fill => Error (msg "memory_fill not supported")
  | BI_memory_copy => Error (msg "memory_copy not supported")
  | BI_memory_init idx => Error (msg "memory_init not supported")
  | BI_data_drop idx => Error (msg "data_drop not supported")
  | _ => Error (msg "not a memory instruction")
  end.


Definition compile_load (ty : number_type) 
                        (trunc : option (packed_type * sx)) 
                        (marg : memarg) 
  : res (list Clight.statement) :=
  match ty with
  | T_i32 =>
    match trunc with
    | None => 
  | T_i64 =>
  | T_f32 =>
  | T_f64 =>
  end.