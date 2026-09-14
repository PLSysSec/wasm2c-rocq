open Compiler

let rec int_of_pos = function
| XH -> 1 | XO p -> 2 * int_of_pos p | XI p -> 2 * int_of_pos p + 1
let int_of_z = function
| Z0 -> 0 | Zpos p -> int_of_pos p | Zneg p -> - (int_of_pos p)

(* C names of external functions, keyed by their Clight identifier *)
let extern_names : (int, string) Hashtbl.t = Hashtbl.create 16

(* libc functions whose prototypes come from the #includes below *)
let libc_functions = ["calloc"; "malloc"; "free"; "memcpy"]

let mem_field_names = [| "data"; "data_end"; "pages"; "min_pages"; "max_pages"; "size" |]

(* decode the 4-bit namespace tag back into a readable name *)
let name_of_ident id =
let n = int_of_pos id in
match Hashtbl.find_opt extern_names n with
| Some s -> s
| None ->
let idx = (n lsr 4) - 1 in
match n land 15 with
| 0 -> Printf.sprintf "f%d" idx       | 1 -> Printf.sprintf "g%d" idx
| 2 -> Printf.sprintf "l%d" idx       | 3 -> Printf.sprintf "s_i32_%d" idx
| 4 -> Printf.sprintf "s_i64_%d" idx  | 5 -> Printf.sprintf "s_f32_%d" idx
| 6 -> Printf.sprintf "s_f64_%d" idx  | 7 -> Printf.sprintf "s_ref_%d" idx
| 8 -> Printf.sprintf "mem%d" idx
| 9 when idx < Array.length mem_field_names -> mem_field_names.(idx)
| 9 -> Printf.sprintf "field%d" idx
| 10 when idx = 0 -> "wasm_memory"
| 10 -> Printf.sprintf "struct%d" idx
| 11 when idx = 1 -> "wasm_instantiate"
| 11 -> Printf.sprintf "rt%d" idx
| 12 -> Printf.sprintf "data%d" idx
| _ -> Printf.sprintf "id%d" n

let base_type = function
| Tvoid -> "void"
| Tint0 (I32, Signed, _) -> "int"
| Tint0 (I32, Unsigned, _) -> "unsigned int"
| Tint0 (I8, Signed, _) -> "signed char"
| Tint0 (I8, Unsigned, _) -> "unsigned char"
| Tint0 (I16, Signed, _) -> "short"
| Tint0 (I16, Unsigned, _) -> "unsigned short"
| Tint0 (IBool, _, _) -> "_Bool"
| Tlong0 (Signed, _) -> "long long"
| Tlong0 (Unsigned, _) -> "unsigned long long"
| Tfloat0 (F32, _) -> "float"   | Tfloat0 (F64, _) -> "double"
| Tstruct (id, _) -> "struct " ^ name_of_ident id
| Tunion (id, _) -> "union " ^ name_of_ident id
| _ -> "/* unsupported type */ int"

(* C declarator syntax: wraps name in the pointer/array/function parts of t *)
let rec string_of_decl t name =
match t with
| Tpointer ((Tarray _ | Tfunction _) as t', _) -> string_of_decl t' ("(*" ^ name ^ ")")
| Tpointer (t', _) -> string_of_decl t' ("*" ^ name)
| Tarray (t', n, _) -> string_of_decl t' (Printf.sprintf "%s[%d]" name (int_of_z n))
| Tfunction (args, ret, _) ->
    let args = if args = [] then "void" else String.concat ", " (List.map string_of_type args) in
    string_of_decl ret (Printf.sprintf "%s(%s)" name args)
| _ -> if name = "" then base_type t else base_type t ^ " " ^ name

and string_of_type t = string_of_decl t ""

let string_of_binop = function
| Oadd -> "+" | Osub -> "-" | Omul -> "*" | Odiv -> "/" | Omod -> "%"
| Oand -> "&" | Oor -> "|" | Oxor -> "^" | Oshl -> "<<" | Oshr -> ">>"
| Oeq -> "==" | One -> "!=" | Olt -> "<" | Ogt -> ">" | Ole -> "<=" | Oge -> ">="

let string_of_unop = function
| Onotbool -> "!" | Onotint -> "~" | Oneg -> "-" | Oabsfloat -> "__builtin_fabs"

let rec string_of_expr = function
| Econst_int (v, _) -> string_of_int (int_of_z v) ^ "U"
| Econst_long (v, _) -> string_of_int (int_of_z v) ^ "ULL"
| Econst_single _ | Econst_float _ -> "0.0 /* float literal */"
| Evar (id, _) | Etempvar (id, _) -> name_of_ident id
| Ederef (e, _) -> Printf.sprintf "(*%s)" (string_of_expr e)
| Eaddrof (e, _) -> Printf.sprintf "(&%s)" (string_of_expr e)
| Eunop (op, e, _) -> Printf.sprintf "(%s(%s))" (string_of_unop op) (string_of_expr e)
| Ebinop (op, a, b, _) ->
    Printf.sprintf "(%s %s %s)" (string_of_expr a) (string_of_binop op) (string_of_expr b)
| Ecast (e, t) -> Printf.sprintf "((%s)%s)" (string_of_type t) (string_of_expr e)
| Efield (e, f, _) -> Printf.sprintf "%s.%s" (string_of_expr e) (name_of_ident f)
| Esizeof (t, _) -> Printf.sprintf "sizeof(%s)" (string_of_type t)
| Ealignof (t, _) -> Printf.sprintf "_Alignof(%s)" (string_of_type t)

let string_of_call f args =
Printf.sprintf "%s(%s)" (string_of_expr f) (String.concat ", " (List.map string_of_expr args))

let rec pp_stmt buf ind s =
let pad = String.make ind ' ' in
match s with
| Sskip -> ()
| Ssequence (a, b) -> pp_stmt buf ind a; pp_stmt buf ind b
| Sset (id, e) -> Printf.bprintf buf "%s%s = %s;\n" pad (name_of_ident id) (string_of_expr e)
| Sassign (l, r) -> Printf.bprintf buf "%s%s = %s;\n" pad (string_of_expr l) (string_of_expr r)
| Scall (None, f, args) -> Printf.bprintf buf "%s%s;\n" pad (string_of_call f args)
| Scall (Some id, f, args) ->
    Printf.bprintf buf "%s%s = %s;\n" pad (name_of_ident id) (string_of_call f args)
| Sbuiltin (None, EF_memcpy (sz, _), _, [dst; src]) ->
    Printf.bprintf buf "%smemcpy(%s, %s, %d);\n" pad (string_of_expr dst) (string_of_expr src) (int_of_z sz)
| Sreturn None -> Printf.bprintf buf "%sreturn;\n" pad
| Sreturn (Some e) -> Printf.bprintf buf "%sreturn %s;\n" pad (string_of_expr e)
| _ -> Printf.bprintf buf "%s/* unsupported statement */\n" pad

let pp_decls buf ind l =
let pad = String.make ind ' ' in
List.iter (fun (id, t) ->
    Printf.bprintf buf "%s%s;\n" pad (string_of_decl t (name_of_ident id))) l

let pp_params l =
if l = [] then "void"
else String.concat ", "
    (List.map (fun (id, t) -> string_of_decl t (name_of_ident id)) l)

let pp_signature id f =
string_of_decl f.fn_return (Printf.sprintf "%s(%s)" (name_of_ident id) (pp_params f.fn_params))

let pp_function buf id f =
Printf.bprintf buf "%s\n{\n" (pp_signature id f);
pp_decls buf 2 f.fn_vars; pp_decls buf 2 f.fn_temps;
Buffer.add_string buf "\n"; pp_stmt buf 2 f.fn_body;
Buffer.add_string buf "}\n\n"

let pp_composite buf = function
| Composite (id, su, members, _) ->
    Printf.bprintf buf "%s %s {\n" (match su with Struct -> "struct" | Union -> "union") (name_of_ident id);
    List.iter (function
    | Member_plain (f, t) -> Printf.bprintf buf "  %s;\n" (string_of_decl t (name_of_ident f))
    | Member_bitfield _ -> Buffer.add_string buf "  /* unsupported bitfield */\n") members;
    Buffer.add_string buf "};\n\n"

let string_of_init = function
| Init_int8 v | Init_int16 v | Init_int32 v -> string_of_int (int_of_z v)
| Init_int64 v -> string_of_int (int_of_z v) ^ "ULL"
| _ -> "0 /* unsupported initializer */"

let pp_globvar buf id v =
let decl = string_of_decl v.gvar_info (name_of_ident id) in
let const = if v.gvar_readonly then "const " else "" in
match v.gvar_init with
| [] -> Printf.bprintf buf "extern %s;\n\n" decl
(* zero-initialized storage, e.g. a module's defined memory *)
| [Init_space _] -> Printf.bprintf buf "%s%s;\n\n" const decl
| inits ->
    Printf.bprintf buf "static %s%s = { %s };\n\n" const decl
      (String.concat ", " (List.map string_of_init inits))

let pp_program p =
List.iter (fun (id, gd) -> match gd with
    | Gfun (External ((EF_external (name, _) | EF_runtime (name, _)), _, _, _)) ->
    Hashtbl.replace extern_names (int_of_pos id) name
    | _ -> ()) p.prog_defs;
let buf = Buffer.create 4096 in
Buffer.add_string buf "/* generated from WebAssembly */\n\n";
Buffer.add_string buf "#include <stdlib.h>\n#include <string.h>\n\n";
List.iter (pp_composite buf) p.prog_types;
(* prototypes, so definitions can appear in any order *)
List.iter (fun (id, gd) -> match gd with
    | Gfun (Internal f) -> Printf.bprintf buf "%s;\n" (pp_signature id f)
    | _ -> ()) p.prog_defs;
Buffer.add_string buf "\n";
List.iter (fun (id, gd) -> match gd with
    | Gfun (Internal f) -> pp_function buf id f
    | Gfun (External (_, _, _, _)) when List.mem (name_of_ident id) libc_functions -> ()
    | Gfun (External (_, args, ret, _)) ->
    Printf.bprintf buf "extern %s;\n"
        (string_of_decl (Tfunction (args, ret, cc_default))
           (name_of_ident id))
    | Gvar v -> pp_globvar buf id v)
    p.prog_defs;
Buffer.contents buf

let render_err m =
String.concat "" (List.map (function
    | MSG s -> s | CTX i -> "$" ^ string_of_int (int_of_pos i)
    | POS i -> string_of_int (int_of_pos i)) m)

let () =
if Array.length Sys.argv < 2 then (prerr_endline "usage: driver <file.wasm>"; exit 2);
let ic = open_in_bin Sys.argv.(1) in
let src = really_input_string ic (in_channel_length ic) in
close_in ic;
match run_parse_module_str src with
| None -> prerr_endline "parse error: not a valid wasm module"; exit 1
| Some m ->
    match compile m with
    | Error e -> prerr_endline ("compile error: " ^ render_err e); exit 1
    | OK p -> print_string (pp_program p)
