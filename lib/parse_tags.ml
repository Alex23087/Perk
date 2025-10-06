(** Parses a [ctags]-generated [tags] file. *)

open Ctype_ast
open C_errors
open Ast

(** The result of parsing a [tags] file.*)
type parse_result =
  | Comment
  | Prototype of string * string * string
  | Typedef of string
  | Other

(** Generates the [tags] file, by first expanding the libraries and merging
    them, and then calling [ctags].*)
let generate_tags lib_paths =
  let libs_expanded =
    Filename.concat (Filename.get_temp_dir_name ()) "libs_expanded.h"
  in
  let status =
    Sys.command
      (Printf.sprintf
         "/usr/local/i386elfgcc/bin/i386-elf-gcc -ffreestanding -m32 \
          -fno-builtin -fno-stack-protector -fno-pic -Wno-error -DVGA_VESA \
          -DHRES=600 -DVRES=400 -DBPP=4 -DWINDOW_DRAG_NORMAL -E -P -dD %s > \
          libs_expanded.h "
         (* "gcc -E -P -dD %s > %s " *)
         (String.concat " " lib_paths)
         libs_expanded)
  in
  if status = 0 then
    let status =
      Sys.command
        (Printf.sprintf "ctags --kinds-C=+p --fields=+Snt %s" libs_expanded)
    in
    if status = 0 then () else failwith "lib expansion failed 1"
  else failwith "lib expansion failed 2"

(** Debug function to print parsed [tags] file*)
let string_of_parse_result pr =
  match pr with
  | Comment -> "comment"
  | Prototype (name, t, signature) ->
      Printf.sprintf "prototype: %s\t%s\t%s" name t signature
  | Typedef n -> Printf.sprintf "typedef %s" n
  | Other -> "other"

(** Reads a file.*)
let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

(** Parses a [tags] line:
    - [p] means that the line represents a prototype
    - [t] means the line represents a typedef. *)
let parse_line s =
  if String.starts_with ~prefix:"!" s then Comment
  else
    let splits = String.split_on_char '\t' s in
    match List.nth splits 3 with
    | "p" ->
        let name = List.nth splits 0 in
        let t = List.nth splits 5 in
        let signature = List.nth splits 6 in
        Prototype (name, t, signature)
    | "t" ->
        let type_name = List.nth splits 0 in
        Typedef type_name
    | _ -> Other

(** Parses the return type of a C prototype *)
let parse_ret_type (s : string) : ctype =
  let lexbuf = Sedlexing.Utf8.from_string s in
  let lexer = Sedlexing.with_tokenizer C_lexer.token lexbuf in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised C_parser.return_type
  in
  try parser lexer with
  | C_lexer.Lexing_error (_pos, _col, msg) -> failwith ("Lexing error: " ^ msg)
  | ParseError e ->
      raise
        (Syntax_error
           ( ( (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum,
               (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
               - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol ),
             ( (snd (Sedlexing.lexing_positions lexbuf)).pos_lnum,
               (snd (Sedlexing.lexing_positions lexbuf)).pos_cnum
               - (snd (Sedlexing.lexing_positions lexbuf)).pos_bol ),
             e ))
  | C_parser.Error ->
      raise
        (Syntax_error
           ( ( (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum,
               (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
               - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol ),
             ( (snd (Sedlexing.lexing_positions lexbuf)).pos_lnum,
               (snd (Sedlexing.lexing_positions lexbuf)).pos_cnum
               - (snd (Sedlexing.lexing_positions lexbuf)).pos_bol ),
             "Unhandled parsing error." ))

(** Parses the signature of a C prototype *)
let parse_signature (s : string) : typed_var list =
  let lexbuf = Sedlexing.Utf8.from_string s in
  let lexer = Sedlexing.with_tokenizer C_lexer.token lexbuf in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised C_parser.signature
  in
  try parser lexer with
  | C_lexer.Lexing_error (_pos, _col, msg) -> failwith ("Lexing error: " ^ msg)
  | ParseError e ->
      raise
        (Syntax_error
           ( ( (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum,
               (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
               - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol ),
             ( (snd (Sedlexing.lexing_positions lexbuf)).pos_lnum,
               (snd (Sedlexing.lexing_positions lexbuf)).pos_cnum
               - (snd (Sedlexing.lexing_positions lexbuf)).pos_bol ),
             e ))
  | C_parser.Error ->
      raise
        (Syntax_error
           ( ( (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum,
               (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
               - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol ),
             ( (snd (Sedlexing.lexing_positions lexbuf)).pos_lnum,
               (snd (Sedlexing.lexing_positions lexbuf)).pos_cnum
               - (snd (Sedlexing.lexing_positions lexbuf)).pos_bol ),
             "Unhandled parsing error." ))

(** Checks if a [parse_result] is a prototype. *)
let is_proto (p : parse_result) =
  match p with Prototype _ -> true | _ -> false

(** Checks if a [parse_result] is a typedef. *)
let is_typedef (p : parse_result) =
  match p with Typedef _ -> true | _ -> false

(** Converts a parse result to a C type. *)
let to_ctypes (p : parse_result) : (string * ctype * typed_var list) option =
  match p with
  | Prototype (name, ret, signature) -> (
      try
        let ctype_sig = parse_signature signature in
        let ctype_ret = parse_ret_type ret in
        Some (name, ctype_ret, ctype_sig)
      with _ ->
        Printf.eprintf "Warning: could not parse prototype for %s\n" name;
        None)
  | _ -> failwith "Trying to convert non-prototype to ctype"

(** Debug function to print C prototype types. *)
let string_of_ctypes ((name, ret, signature) : string * ctype * typed_var list)
    =
  Printf.sprintf "%s : (%s) -> %s" name (show_ctype ret)
    (List.map show_typed_var signature |> String.concat " , ")

let get_typedef p = match p with Typedef s -> s | _ -> failwith ""
let is_spec_type s = match s with CBaseSort _ -> true | _ -> false
let is_pointer_level s = match s with PtrLevel _ -> true | _ -> false

(** Given a C specifier list, converts it to a [stdint] type.*)
let solve_specifiers (s : specifier list) =
  let present x = List.mem x s in
  match (present Short, present Long, present Signed, present Unsigned) with
  | true, false, true, false -> CBaseSort "int16_t" (* short signed *)
  | true, false, false, true -> CBaseSort "uint16_t" (* short unsigned *)
  | false, true, true, false -> CBaseSort "int64_t" (* long signed *)
  | false, true, false, true -> CBaseSort "uint64_t" (* long unsigned *)
  | false, false, false, true -> CBaseSort "uint32_t" (* signed *)
  | false, false, true, false -> CBaseSort "int32_t" (* unsigned *)
  | true, false, false, false -> CBaseSort "int16_t" (* short *)
  | false, true, false, false -> CBaseSort "int64_t" (* long *)
  | false, false, false, false -> CBaseSort "int" (* no specifiers *)
  | _ ->
      failwith
        (Printf.sprintf "Invalid or ambiguous specifier combination: %s"
           (List.map show_specifier s |> String.concat ", "))

(** Given a specifier list, returns the corresponding type. *)
let get_type_from_spec_list sl =
  let sorts = List.filter is_spec_type sl in
  let len = List.length sorts in
  if len = 1 then
    let sort = List.hd sorts in
    if sort = CBaseSort "int" then solve_specifiers sl else sort
  else if len = 0 then solve_specifiers sl
  else failwith "Illegal type - more than one base type"

let remove_tags () = if not !Utils.verbose then Sys.command "rm tags" |> ignore

let remove_libs_expanded () =
  let libs_expanded =
    Filename.concat (Filename.get_temp_dir_name ()) "libs_expanded.h"
  in
  if not !Utils.verbose then
    Sys.command (Printf.sprintf "rm %s" libs_expanded) |> ignore

(** Given a basic C sort, returns the corresponding base type *)
let perktype_of_sort s : perktype_partial =
  match s with CBaseSort x -> Basetype x | _ -> failwith ""

(** Wraps a type in pointers, a positive number of times. *)
let rec wrap_perktype_in_ptrs n (t : perktype) : perktype_partial =
  if n = 0 then failwith "should not call with 0"
  else if n = 1 then Pointertype t
  else Pointertype ([], wrap_perktype_in_ptrs (n - 1) t, [])

(** Transforms C qualifiers to perk qualifiers *)
let qual_to_perkqual : qualifier -> perktype_qualifier = function
  | Const -> Const
  | Volatile -> Volatile
  | Restrict -> Restrict

(** Transforms a c type to a perk type *)
let perktype_of_ctype c =
  let rec aux (sl : specifier list) (curr_sort : specifier list)
      (perkquals : perktype_qualifier list) : perktype =
    match sl with
    | ((Long | Short | Signed | Unsigned | CBaseSort _) as s) :: rest ->
        aux rest (curr_sort @ [ s ]) perkquals
    | Qual q :: rest -> aux rest curr_sort (perkquals @ [ qual_to_perkqual q ])
    | PtrLevel n :: rest ->
        ([], wrap_perktype_in_ptrs n (aux rest curr_sort []), perkquals)
    | [] ->
        ([], get_type_from_spec_list curr_sort |> perktype_of_sort, perkquals)
  in
  match c with CBaseType specifiers -> aux (List.rev specifiers) [] []

(** Given a C prototype argument, returns its type. *)
let perktype_of_arg tv : perktype =
  match tv with
  | TypedVar (t, IdenDecl _name) -> perktype_of_ctype t
  | Ellipsis -> ([], Vararg, [])

(** Returns the perk types of each C prototype in the tags file. *)

let get_prototype_types () : (string * perktype) list =
  let lines = read_file "tags" in
  let parsed_lines = List.map parse_line lines in
  let typedef_vars =
    parsed_lines |> List.filter is_typedef |> List.map get_typedef
  in
  let proto_types =
    C_lexer.type_names := !C_lexer.type_names @ typedef_vars;
    parsed_lines |> List.filter is_proto |> List.map to_ctypes
    |> List.filter Option.is_some |> List.map Option.get
  in

  let proto_type_to_funtype (name, ctyp, args) =
    let ret_type = perktype_of_ctype ctyp in
    let arg_types = List.map perktype_of_arg args in
    (name, ([], Funtype (arg_types, ret_type), []))
  in
  List.map proto_type_to_funtype proto_types
