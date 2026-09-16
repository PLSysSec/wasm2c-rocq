open Compiler

let rec int_of_pos = function
| XH -> 1 | XO p -> 2 * int_of_pos p | XI p -> 2 * int_of_pos p + 1
let int_of_z = function
| Z0 -> 0 | Zpos p -> int_of_pos p | Zneg p -> - (int_of_pos p)

(* C names of external functions, keyed by their Clight identifier *)
let extern_names : (int, string) Hashtbl.t = Hashtbl.create 16

(* libc functions whose prototypes come from the #includes below *)
let libc_functions = ["calloc"; "malloc"; "free"; "memcpy"; "realloc"; "memset"]

let field_names =
  [| "data"; "pages"; "min_pages"; "max_pages"; "size";
     "mem"; "trapflag"; "globals" |]

(* scratch cells and temps from Memory.v, in ident_of_scratch order *)
let scratch_names =
  [| "scratch_u8"; "scratch_u16"; "scratch_u32"; "scratch_u64";
     "scratch_f32"; "scratch_f64"; "scratch_ptr" |]

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
| 8 when idx = 0 -> "inst"
| 8 -> Printf.sprintf "inst%d" idx
| 9 when idx < Array.length field_names -> field_names.(idx)
| 9 -> Printf.sprintf "field%d" idx
| 10 when idx = 0 -> "wasm_instance"
| 10 when idx = 1 -> "wasm_memory"
| 10 -> Printf.sprintf "struct%d" idx
| 11 -> Printf.sprintf "rt%d" idx
| 12 -> Printf.sprintf "data%d" idx
| 13 when idx < Array.length scratch_names -> scratch_names.(idx)
| 13 -> Printf.sprintf "scratch%d" idx
| 14 when idx = 0 -> "wasm_instantiate"
| 14 -> Printf.sprintf "builtin%d" idx
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
| Efield (Ederef (e, _), f, _) ->
    Printf.sprintf "%s->%s" (string_of_expr e) (name_of_ident f)
| Efield (e, f, _) ->Printf.sprintf "%s.%s" (string_of_expr e) (name_of_ident f)
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
| Sifthenelse (c, a, b) ->
    Printf.bprintf buf "%sif (%s) {\n" pad (string_of_expr c);
    pp_stmt buf (ind + 2) a;
    (match b with
     | Sskip -> Printf.bprintf buf "%s}\n" pad
     | _ ->
       Printf.bprintf buf "%s} else {\n" pad;
       pp_stmt buf (ind + 2) b;
       Printf.bprintf buf "%s}\n" pad)
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

let register_extern_names p =
List.iter (fun (id, gd) -> match gd with
    | Gfun (External ((EF_external (name, _) | EF_runtime (name, _)), _, _, _)) ->
    Hashtbl.replace extern_names (int_of_pos id) name
    | _ -> ()) p.prog_defs

let pp_program p =
register_extern_names p;
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

(* Clight AST dump, in Rocq-like constructor syntax *)
module Ast = struct
open Format

(* exact decimal rendering of a positive, since 64-bit constants overflow int *)
let string_of_pos p =
let rec bits acc = function
  | XH -> true :: acc | XO p -> bits (false :: acc) p | XI p -> bits (true :: acc) p in
let digits = ref [0] in (* little-endian base 10 *)
List.iter (fun b ->
    let carry = ref (if b then 1 else 0) in
    digits := List.map (fun d -> let v = 2 * d + !carry in carry := v / 10; v mod 10) !digits;
    if !carry > 0 then digits := !digits @ [!carry]) (bits [] p);
String.concat "" (List.rev_map string_of_int !digits)

let string_of_z = function
| Z0 -> "0" | Zpos p -> string_of_pos p | Zneg p -> "-" ^ string_of_pos p

let pos ppf p = fprintf ppf "%s%%positive" (string_of_pos p)
let z ppf = function
| Zneg p -> fprintf ppf "(-%s)%%Z" (string_of_pos p)
| v -> fprintf ppf "%s%%Z" (string_of_z v)
let n ppf = function N0 -> fprintf ppf "0%%N" | Npos p -> fprintf ppf "%s%%N" (string_of_pos p)
let bool ppf b = pp_print_string ppf (if b then "true" else "false")
let str ppf s = fprintf ppf "%S" s
(* identifiers below 16 fall outside the namespace encoding, so show them raw *)
let ident ppf id = if int_of_pos id < 16 then pos ppf id else fprintf ppf "_%s" (name_of_ident id)

(* constructor application: Ctor a b c *)
let app ppf name args =
if args = [] then pp_print_string ppf name
else begin
  fprintf ppf "@[<hov 2>%s" name;
  List.iter (fun a -> fprintf ppf "@ %t" a) args;
  fprintf ppf "@]"
end
let arg pp x ppf = pp ppf x
(* parenthesized argument, for anything that may itself be an application *)
let parg pp x ppf = fprintf ppf "(%a)" pp x
let paren atomic pp x ppf = if atomic x then pp ppf x else parg pp x ppf

let list pp ppf l =
if l = [] then pp_print_string ppf "nil"
else fprintf ppf "@[<hv 2>[ %a@;<1 -2>]@]"
    (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";@ ") pp) l

(* wrap is arg or parg, depending on whether pp prints atoms *)
let option wrap pp ppf = function
| None -> pp_print_string ppf "None"
| Some x -> app ppf "Some" [wrap pp x]
let popt wrap pp = paren (( = ) None) (option wrap pp)

let record ppf fields =
fprintf ppf "@[<hv 3>{| %a@;<1 -3>|}@]"
  (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";@ ")
     (fun ppf (name, pp) -> fprintf ppf "@[<hov 2>%s :=@ %t@]" name pp)) fields

let pair ppa ppb ppf (a, b) = fprintf ppf "@[<hov 1>(%a,@ %a)@]" ppa a ppb b

let intsize ppf s = pp_print_string ppf (match s with I8 -> "I8" | I16 -> "I16" | I32 -> "I32" | IBool -> "IBool")
let signedness ppf s = pp_print_string ppf (match s with Signed -> "Signed" | Unsigned -> "Unsigned")
let floatsize ppf s = pp_print_string ppf (match s with F32 -> "F32" | F64 -> "F64")

let attr ppf a =
if a = noattr then pp_print_string ppf "noattr"
else record ppf ["attr_volatile", arg bool a.attr_volatile;
                 "attr_alignas", arg (option arg n) a.attr_alignas]

let callconv ppf c =
if c = cc_default then pp_print_string ppf "cc_default"
else record ppf ["cc_vararg", arg (option arg z) c.cc_vararg;
                 "cc_unproto", arg bool c.cc_unproto;
                 "cc_structret", arg bool c.cc_structret]

let rec ty ppf = function
| Tvoid -> pp_print_string ppf "Tvoid"
| Tint0 (sz, sg, a) -> app ppf "Tint" [arg intsize sz; arg signedness sg; arg attr a]
| Tlong0 (sg, a) -> app ppf "Tlong" [arg signedness sg; arg attr a]
| Tfloat0 (sz, a) -> app ppf "Tfloat" [arg floatsize sz; arg attr a]
| Tpointer (t, a) -> app ppf "Tpointer" [pty t; arg attr a]
| Tarray (t, len, a) -> app ppf "Tarray" [pty t; arg z len; arg attr a]
| Tfunction (args, ret, cc) -> app ppf "Tfunction" [arg (list ty) args; pty ret; arg callconv cc]
| Tstruct (id, a) -> app ppf "Tstruct" [arg ident id; arg attr a]
| Tunion (id, a) -> app ppf "Tunion" [arg ident id; arg attr a]

and pty t = paren (( = ) Tvoid) ty t

let unop ppf op = pp_print_string ppf (match op with
| Onotbool -> "Onotbool" | Onotint -> "Onotint" | Oneg -> "Oneg" | Oabsfloat -> "Oabsfloat")

let binop ppf op = pp_print_string ppf (match op with
| Oadd -> "Oadd" | Osub -> "Osub" | Omul -> "Omul" | Odiv -> "Odiv" | Omod -> "Omod"
| Oand -> "Oand" | Oor -> "Oor" | Oxor -> "Oxor" | Oshl -> "Oshl" | Oshr -> "Oshr"
| Oeq -> "Oeq" | One -> "One" | Olt -> "Olt" | Ogt -> "Ogt" | Ole -> "Ole" | Oge -> "Oge")

let float ppf = function
| B754_zero s -> app ppf "B754_zero" [arg bool s]
| B754_infinity s -> app ppf "B754_infinity" [arg bool s]
| B754_nan (s, pl) -> app ppf "B754_nan" [arg bool s; arg pos pl]
| B754_finite (s, m, e) -> app ppf "B754_finite" [arg bool s; arg pos m; arg z e]

let rec expr ppf = function
| Econst_int (v, t) -> app ppf "Econst_int" [arg z v; pty t]
| Econst_float (v, t) -> app ppf "Econst_float" [parg float v; pty t]
| Econst_single (v, t) -> app ppf "Econst_single" [parg float v; pty t]
| Econst_long (v, t) -> app ppf "Econst_long" [arg z v; pty t]
| Evar (id, t) -> app ppf "Evar" [arg ident id; pty t]
| Etempvar (id, t) -> app ppf "Etempvar" [arg ident id; pty t]
| Ederef (e, t) -> app ppf "Ederef" [parg expr e; pty t]
| Eaddrof (e, t) -> app ppf "Eaddrof" [parg expr e; pty t]
| Eunop (op, e, t) -> app ppf "Eunop" [arg unop op; parg expr e; pty t]
| Ebinop (op, a, b, t) -> app ppf "Ebinop" [arg binop op; parg expr a; parg expr b; pty t]
| Ecast (e, t) -> app ppf "Ecast" [parg expr e; pty t]
| Efield (e, f, t) -> app ppf "Efield" [parg expr e; arg ident f; pty t]
| Esizeof (t, t') -> app ppf "Esizeof" [pty t; pty t']
| Ealignof (t, t') -> app ppf "Ealignof" [pty t; pty t']

let typ ppf t = pp_print_string ppf (match t with
| Tint -> "AST.Tint" | Tfloat -> "AST.Tfloat" | Tlong -> "AST.Tlong"
| Tsingle -> "AST.Tsingle" | Tany32 -> "AST.Tany32" | Tany64 -> "AST.Tany64")

let xtype ppf t = pp_print_string ppf (match t with
| Xbool -> "Xbool" | Xint8signed -> "Xint8signed" | Xint8unsigned -> "Xint8unsigned"
| Xint16signed -> "Xint16signed" | Xint16unsigned -> "Xint16unsigned" | Xint -> "Xint"
| Xfloat -> "Xfloat" | Xlong -> "Xlong" | Xsingle -> "Xsingle" | Xptr -> "Xptr"
| Xany32 -> "Xany32" | Xany64 -> "Xany64" | Xvoid -> "Xvoid")

let signature ppf s =
record ppf ["sig_args", arg (list xtype) s.sig_args;
            "sig_res", arg xtype s.sig_res;
            "sig_cc", arg callconv s.sig_cc]

let chunk ppf c = pp_print_string ppf (match c with
| Mbool -> "Mbool" | Mint8signed -> "Mint8signed" | Mint8unsigned -> "Mint8unsigned"
| Mint16signed -> "Mint16signed" | Mint16unsigned -> "Mint16unsigned" | Mint32 -> "Mint32"
| Mint64 -> "Mint64" | Mfloat32 -> "Mfloat32" | Mfloat64 -> "Mfloat64"
| Many32 -> "Many32" | Many64 -> "Many64")

let external_function ppf = function
| EF_external (name, s) -> app ppf "EF_external" [arg str name; arg signature s]
| EF_builtin (name, s) -> app ppf "EF_builtin" [arg str name; arg signature s]
| EF_runtime (name, s) -> app ppf "EF_runtime" [arg str name; arg signature s]
| EF_vload c -> app ppf "EF_vload" [arg chunk c]
| EF_vstore c -> app ppf "EF_vstore" [arg chunk c]
| EF_malloc -> pp_print_string ppf "EF_malloc"
| EF_free -> pp_print_string ppf "EF_free"
| EF_memcpy (sz, al) -> app ppf "EF_memcpy" [arg z sz; arg z al]
| EF_annot (k, s, ts) -> app ppf "EF_annot" [arg pos k; arg str s; arg (list typ) ts]
| EF_annot_val (k, s, t) -> app ppf "EF_annot_val" [arg pos k; arg str s; arg typ t]
| EF_inline_asm (s, sg, l) -> app ppf "EF_inline_asm" [arg str s; arg signature sg; arg (list str) l]
| EF_debug (k, id, ts) -> app ppf "EF_debug" [arg pos k; arg ident id; arg (list typ) ts]

let rec stmt ppf = function
| Sskip -> pp_print_string ppf "Sskip"
| Sassign (l, r) -> app ppf "Sassign" [parg expr l; parg expr r]
| Sset (id, e) -> app ppf "Sset" [arg ident id; parg expr e]
| Scall (res, f, args) -> app ppf "Scall" [popt arg ident res; parg expr f; arg (list expr) args]
| Sbuiltin (res, ef, ts, args) ->
    app ppf "Sbuiltin" [popt arg ident res; parg external_function ef; arg (list ty) ts; arg (list expr) args]
| Ssequence _ as s -> fprintf ppf "@[<v 0>%a@]" seq s
| Sifthenelse (c, a, b) -> fprintf ppf "@[<v 2>Sifthenelse (%a)@ (%a)@ (%a)@]" expr c stmt a stmt b
| Sloop (a, b) -> fprintf ppf "@[<v 2>Sloop@ (%a)@ (%a)@]" stmt a stmt b
| Sbreak -> pp_print_string ppf "Sbreak"
| Scontinue -> pp_print_string ppf "Scontinue"
| Sreturn e -> app ppf "Sreturn" [popt parg expr e]
| Sswitch (e, ls) -> fprintf ppf "@[<v 2>Sswitch (%a)@ (%a)@]" expr e labeled ls
| Slabel (l, s) -> fprintf ppf "@[<v 2>Slabel %a@ (%a)@]" ident l stmt s
| Sgoto l -> app ppf "Sgoto" [arg ident l]

(* right-nested sequences are kept at one indentation level instead of drifting right *)
and seq ppf = function
| Ssequence (a, b) ->
    let tail = match b with Ssequence _ -> seq | _ -> stmt in
    fprintf ppf "@[<v 2>Ssequence@ (%a)@]@ (%a)" stmt a tail b
| s -> stmt ppf s

and labeled ppf = function
| LSnil -> pp_print_string ppf "LSnil"
| LScons (c, s, rest) -> fprintf ppf "@[<v 2>LScons (%a)@ (%a)@ (%a)@]" (option arg z) c stmt s labeled rest

let function_ ppf f =
fprintf ppf "@[<v 3>{| fn_return := %a;@ fn_callconv := %a;@ fn_params := %a;@ fn_vars := %a;@ fn_temps := %a;@ fn_body :=@;<1 2>%a@;<1 -3>|}@]"
  ty f.fn_return callconv f.fn_callconv
  (list (pair ident ty)) f.fn_params (list (pair ident ty)) f.fn_vars
  (list (pair ident ty)) f.fn_temps stmt f.fn_body

let fundef ppf = function
| Internal f -> fprintf ppf "@[<v 2>Internal@ %a@]" function_ f
| External (ef, args, ret, cc) ->
    app ppf "External" [parg external_function ef; arg (list ty) args; pty ret; arg callconv cc]

let init_data ppf = function
| Init_int8 v -> app ppf "Init_int8" [arg z v]
| Init_int16 v -> app ppf "Init_int16" [arg z v]
| Init_int32 v -> app ppf "Init_int32" [arg z v]
| Init_int64 v -> app ppf "Init_int64" [arg z v]
| Init_float32 v -> app ppf "Init_float32" [parg float v]
| Init_float64 v -> app ppf "Init_float64" [parg float v]
| Init_space v -> app ppf "Init_space" [arg z v]
| Init_addrof (id, ofs) -> app ppf "Init_addrof" [arg ident id; arg z ofs]

let globdef ppf = function
| Gfun f -> fprintf ppf "@[<v 2>Gfun@ (%a)@]" fundef f
| Gvar v ->
    fprintf ppf "@[<hv 2>Gvar@ %a@]" record
      ["gvar_info", arg ty v.gvar_info;
       "gvar_init", arg (list init_data) v.gvar_init;
       "gvar_readonly", arg bool v.gvar_readonly;
       "gvar_volatile", arg bool v.gvar_volatile]

let member ppf = function
| Member_plain (id, t) -> app ppf "Member_plain" [arg ident id; pty t]
| Member_bitfield (id, sz, sg, a, w, padding) ->
    app ppf "Member_bitfield" [arg ident id; arg intsize sz; arg signedness sg; arg attr a; arg z w; arg bool padding]

let composite ppf (Composite (id, su, members, a)) =
app ppf "Composite"
  [arg ident id; arg pp_print_string (match su with Struct -> "Struct" | Union -> "Union");
   arg (list member) members; arg attr a]

let program ppf p =
(* numeric values of identifiers bound at top level or in composites *)
let ids = List.map fst p.prog_defs
  @ List.concat_map (fun (Composite (id, _, ms, _)) ->
      id :: List.map (function Member_plain (f, _) | Member_bitfield (f, _, _, _, _, _) -> f) ms)
    p.prog_types in
let ids = List.sort_uniq (fun a b -> compare (int_of_pos a) (int_of_pos b)) ids in
fprintf ppf "@[<v 0>(* generated from WebAssembly *)@ @ (* identifiers:";
List.iter (fun id -> fprintf ppf "@   %a = %a" ident id pos id) ids;
fprintf ppf "@ *)@ @ ";
fprintf ppf "@[<v 2>{| prog_defs :=@ %a;@ prog_public := %a;@ prog_main := %a;@ prog_types :=@ %a@;<1 -2>|}@]@]@."
  (list (fun ppf (id, gd) -> fprintf ppf "@[<v 1>(%a,@ %a)@]" ident id globdef gd)) p.prog_defs
  (list ident) p.prog_public ident p.prog_main
  (list composite) p.prog_types
end

let render_err m =
String.concat "" (List.map (function
    | MSG s -> s | CTX i -> "$" ^ string_of_int (int_of_pos i)
    | POS i -> string_of_int (int_of_pos i)) m)

let () =
let usage () = prerr_endline "usage: wasm2c [--ast] <file.wasm>"; exit 2 in
let ast, file = match List.tl (Array.to_list Sys.argv) with
  | [f] when f <> "--ast" -> false, f
  | ["--ast"; f] | [f; "--ast"] -> true, f
  | _ -> usage () in
let ic = open_in_bin file in
let src = really_input_string ic (in_channel_length ic) in
close_in ic;
match run_parse_module_str src with
| None -> prerr_endline "parse error: not a valid wasm module"; exit 1
| Some m ->
    match compile m with
    | Error e -> prerr_endline ("compile error: " ^ render_err e); exit 1
    | OK p when ast ->
        register_extern_names p;
        Format.set_margin 100;
        Format.set_max_indent 90;
        Ast.program Format.std_formatter p
    | OK p -> print_string (pp_program p)
