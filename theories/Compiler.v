From Wasm Require Import datatypes operations.
From Stdlib Require Import PArith.
From compcert Require cfrontend.Clight cfrontend.Ctypes common.AST common.Errors.
From compcert Require Import export.Ctypesdefs.
Require Import List.
Import ListNotations.

Definition wasm_type_to_clight_type (t: value_type) : option Ctypes.type :=
  match t with
  | T_num T_i32  => Some tuint        (* always 32 bits *)
  | T_num T_i64  => Some tulong       (* always 64 bits*)
  | T_num T_f32  => Some tfloat
  | T_num T_f64  => Some tdouble
  | T_ref _      => Some (tptr tvoid) (* don't care if it's a funcref or extern ref *)
  | T_vec T_v128 => None              (* no scalar equivalent *)
  | T_bot        => None
  end.

Fixpoint wasm_params_to_clight_params (next: AST.ident) (ts: list value_type) : option (list (AST.ident * Ctypes.type)) :=
  match ts with
  | nil => Some nil
  | t :: ts' =>
    match wasm_type_to_clight_type t, wasm_params_to_clight_params (Pos.succ next) ts' with
    | Some ty, Some rest => Some ((next, ty) :: rest)
    | _, _ => None
    end
  end.

Definition wasm_return_to_clight_return (ts: list value_type) : option Ctypes.type :=
  match ts with
  | nil      => Some tvoid
  | t :: nil => wasm_type_to_clight_type t
  | _ :: _ :: _ => None (* multi-value (i think can't happen in wasm 1.0) *)
  end.

(* return type, calling_convention, params*)
Definition clight_of_functype (next: AST.ident) (tf: function_type) 
    : option (Ctypes.type * AST.calling_convention * list(AST.ident * Ctypes.type)) :=
  let 'Tf ts1 ts2 := tf in
  match wasm_return_to_clight_return ts2, wasm_params_to_clight_params next ts1 with
  | Some ret, Some ps => Some (ret, AST.cc_default, ps)
  | _, _ => None
  end.

(* 
wasm2c represents top two vars in stack

i32.const 3
 
=> int32_t i0 = 3;

*)

Definition compiler_state : Record {
  next : Nat
}

Definition instr_to_statement (cs : compiler_state) (instr: basic_instruction) : option (list Clight.statement * compiler_state) :=
  match instr with
  | BI_const_num val => match val with
    | VAL_int32 num => ([Sdo (Eassign (Evar cs.next int32_t) (Eval num int32_t))], increment_csnext(cs))
    | VAL_int64  =>  ([Sdo (Eassign (Evar cs.next int64_t) (Eval num int64_t))], increment_csnext(cs))
    | VAL_float32 =>  ([Sdo (Eassign (Evar cs.next f32_t) (Eval num f32_t))], increment_csnext(cs))
    | VAL_float64 =>  ([Sdo (Eassign (Evar cs.next f64_t) (Eval num f64_t))], increment_csnext(cs))
  | BI_unop => None
  | BI_binop op => match op with
    | Binop_i op' => match op' with
      | BOI_add => ([Sdo (Eassign (Evar (cs.next - 2) i32_t) (Ebinop add (Evar (cs.next - 1)) (cs.next - 2) ))])
      | BOI_sub => ([Sdo (Eassign (Evar (cs.next - 2) i32_t) (Ebinop sub (Evar (cs.next - 1)) (cs.next - 2) ))])
      | None
      end
    | _ => None
    end
  | BI_testop
  | BI_relop
  | BI_cvtop
  (* no simd in vanilla compcert *)
  | BI_const_vec => None
  | BI_vunop => None
  | BI_vbinop => None
  | BI_vternop => None
  | BI_vtestop => None
  | BI_vshiftop => None
  | BI_splat_vec => None
  | BI_extract_vec => None
  | BI_replace_vec => None
  (* end simd *)
  | BI_ref_null
  | BI_ref_is_null
  | BI_ref_func
  | BI_drop
  | BI_select
  | BI_local_get
  | BI_local_set
  | BI_local_tee
  | BI_global_get
  | BI_global_set
  | BI_table_get
  | BI_table_set
  | BI_table_size
  | BI_table_grow
  | BI_table_fill
  | BI_table_copy
  | BI_table_init
  | BI_elem_drop
  | BI_load
  | BI_load_vec
  | BI_load_vec_lane
  | BI_store
  | BI_store_vec
  | BI_store_vec_lane : vwidth -> memarg -> laneidx -> basic_instruction
  | BI_memory_size
  | BI_memory_grow
  | BI_memory_fill
  | BI_memory_copy
  | BI_memory_init: dataidx -> basic_instruction
  | BI_data_drop: dataidx -> basic_instruction
(** std-doc:
Instructions in this group affect the flow of control.
**)
  | BI_nop
  | BI_unreachable
  | BI_block : block_type -> list basic_instruction -> basic_instruction
  | BI_loop : block_type -> list basic_instruction -> basic_instruction
  | BI_if : block_type -> list basic_instruction -> list basic_instruction -> basic_instruction
  | BI_br : labelidx -> basic_instruction
  | BI_br_if : labelidx -> basic_instruction
  | BI_br_table : list labelidx -> labelidx -> basic_instruction
  | BI_return
  | BI_call : funcidx -> basic_instruction
  | BI_call_indirect : tableidx -> typeidx -> basic_instruction
  | BI_return_call : funcidx -> basic_instruction                                          
  | BI_return_call_indirect : tableidx -> typeidx -> basic_instruction    
  end.


Definition seq_of_list (l: list Clight.statement) : Clight.statement :=
  List.fold_right Clight.Ssequence Clight.Sskip l.

Definition compile_body (body: expr) : option Clight.statement :=
  Some (seq_of_list (List.map instr_to_statement body)).

(*  Clight.function := { 
      fn_return: type;                // Can figure out from function type
      fn_callconv: calling_convention := {
        cc_vararg: option Z; // variable args? I think should be None
        cc_unproto: bool;    // <<old-style unprototyped function>> I assume just false
        cc_structret: bool   // I think never returns a struct? Unless multi return
      }
      fn_params: list (ident * type); // Types can come from function type, idents come from stack?
      fn_vars: list (ident * type);   // Addressable local variables -- [] I think bc wasm locals non-addressable
      fn_temps: list (ident * type);  // Non-addressable local variables func.(modfunc_locals) i think
      fn_body: statement              // func.(modfunc_body)
    }

    func_type := lookup_N m.(mod_types) func.(modfunc_type)
*)


(*
    module_func defined in WasmCert-Coq/theories/datatypes.v:639
    Clight.function defined in CompCert/cfrontend/Clight.v:135
*)
Definition compile_func (m : module) (func : module_func) : option Clight.function :=
  match lookup_N m.(mod_types) func.(modfunc_type) with
  | Some func_type =>
    match clight_of_functype 1%positive func_type with                         (* need to figure out what ident to start at*)
    | Some (ret_type, cc, params) => 
      match wasm_params_to_clight_params 1%positive func.(modfunc_locals) with (* need to start at an ident after params*)
      | Some locals =>
        match compile_body func.(modfunc_body) with
        | Some body => Some (Clight.mkfunction ret_type cc params nil locals body)
        | _ => None
        end
      | _ => None
      end
    | _ => None
    end
  | _ => None
  end.
(*
    - module defined in WasmCert-Coq/theories/datatypes.v:740
    - Clight.program defined in CompCert/cfrontend/Ctypes.v:1545 (res discharges
      a proof obligation)
*)
Definition compile (m : module) : Errors.res Clight.program.
Admitted.
    (* shape of wasm2c compile is

        write_header(module)
        write_source(module)
        return
    
    *)
