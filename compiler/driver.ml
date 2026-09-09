open Compiler

let rec int_of_pos = function
| XH -> 1 | XO p -> 2 * int_of_pos p | XI p -> 2 * int_of_pos p + 1
let int_of_z = function
| Z0 -> 0 | Zpos p -> int_of_pos p | Zneg p -> - (int_of_pos p)

(* decode the 3-bit namespace tag back into a readable name *)
let name_of_ident id =
let n = int_of_pos id in
let idx = (n lsr 3) - 1 in
match n land 7 with
| 0 -> Printf.sprintf "f%d" idx      | 1 -> Printf.sprintf "g%d" idx
| 2 -> Printf.sprintf "l%d" idx      | 3 -> Printf.sprintf "s_i32_%d" idx
| 4 -> Printf.sprintf "s_i64_%d" idx | 5 -> Printf.sprintf "s_f32_%d" idx
| 6 -> Printf.sprintf "s_f64_%d" idx | _ -> Printf.sprintf "s_ref_%d" idx

let rec string_of_type = function
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
| Tpointer (t, _) -> string_of_type t ^ " *"
| _ -> "/* unsupported type */ int"

let string_of_binop = function
| Oadd -> "+" | Osub -> "-" | Omul -> "*" | Odiv -> "/" | Omod -> "%"
| Oand -> "&" | Oor -> "|" | Oxor -> "^" | Oshl -> "<<" | Oshr -> ">>"
| Oeq -> "==" | One -> "!=" | Olt -> "<" | Ogt -> ">" | Ole -> "<=" | Oge -> ">="

let rec string_of_expr = function
| Econst_int (v, _) -> string_of_int (int_of_z v) ^ "U"
| Econst_long (v, _) -> string_of_int (int_of_z v) ^ "ULL"
| Econst_single _ | Econst_float _ -> "0.0 /* float literal */"
| Evar (id, _) | Etempvar (id, _) -> name_of_ident id
| Ebinop (op, a, b, _) ->
    Printf.sprintf "(%s %s %s)" (string_of_expr a) (string_of_binop op) (string_of_expr b)
| Ecast (e, t) -> Printf.sprintf "(%s)%s" (string_of_type t) (string_of_expr e)
| _ -> "/* unsupported expr */ 0"

let rec pp_stmt buf ind s =
let pad = String.make ind ' ' in
match s with
| Sskip -> ()
| Ssequence (a, b) -> pp_stmt buf ind a; pp_stmt buf ind b
| Sset (id, e) -> Printf.bprintf buf "%s%s = %s;\n" pad (name_of_ident id) (string_of_expr e)
| Sassign (l, r) -> Printf.bprintf buf "%s%s = %s;\n" pad (string_of_expr l) (string_of_expr r)
| Sreturn None -> Printf.bprintf buf "%sreturn;\n" pad
| Sreturn (Some e) -> Printf.bprintf buf "%sreturn %s;\n" pad (string_of_expr e)
| _ -> Printf.bprintf buf "%s/* unsupported statement */\n" pad

let pp_decls buf ind l =
let pad = String.make ind ' ' in
List.iter (fun (id, t) ->
    Printf.bprintf buf "%s%s %s;\n" pad (string_of_type t) (name_of_ident id)) l

let pp_params l =
if l = [] then "void"
else String.concat ", "
    (List.map (fun (id, t) -> string_of_type t ^ " " ^ name_of_ident id) l)

let pp_function buf id f =
Printf.bprintf buf "%s %s(%s)\n{\n"
    (string_of_type f.fn_return) (name_of_ident id) (pp_params f.fn_params);
pp_decls buf 2 f.fn_vars; pp_decls buf 2 f.fn_temps;
Buffer.add_string buf "\n"; pp_stmt buf 2 f.fn_body;
Buffer.add_string buf "}\n\n"

let pp_program p =
let buf = Buffer.create 4096 in
Buffer.add_string buf "/* generated from WebAssembly */\n\n";
List.iter (fun (id, gd) -> match gd with
    | Gfun (Internal f) -> pp_function buf id f
    | Gfun (External (_, args, ret, _)) ->
    Printf.bprintf buf "extern %s %s(%s);\n" (string_of_type ret) (name_of_ident id)
        (if args = [] then "void" else String.concat ", " (List.map string_of_type args))
    | Gvar _ -> Buffer.add_string buf "/* global variable (unsupported) */\n")
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