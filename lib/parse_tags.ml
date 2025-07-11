open Ctype_ast
open C_errors
open Ast

type parse_result = Comment | Prototype of string * string * string | Typedef of string | Other

let string_of_parse_result pr = match pr with
    | Comment -> "comment"
    | Prototype (name, t, signature) -> Printf.sprintf "prototype: %s\t%s\t%s" name t signature
    | Typedef n -> Printf.sprintf "typedef %s" n
    | Other -> "other"

let read_file filename = 
    let lines = ref [] in
    let chan = open_in filename in
    try
    while true; do
        lines := input_line chan :: !lines
    done; !lines
    with End_of_file ->
    close_in chan;
    List.rev !lines ;;
    

let parse_line s = if String.starts_with ~prefix:"!" s then Comment
else let splits = String.split_on_char '\t' s in
match List.nth splits 3 with
    | "p" -> 
        let name = List.nth splits 0 in
        let t = List.nth splits 5 in
        let signature = List.nth splits 6 in
        Prototype(name, t, signature)
    | "t" -> let type_name = List.nth splits 0 in Typedef (type_name)
    | _ -> Other

let parse_ret_type (s : string) : ctype =
    let lexbuf = Sedlexing.Utf8.from_string s in
    let lexer = Sedlexing.with_tokenizer C_lexer.token lexbuf in
    let parser =
      MenhirLib.Convert.Simplified.traditional2revised C_parser.return_type in
    try
      parser lexer
    with
    | C_lexer.Lexing_error  (_pos, _col, msg) ->
        failwith ("Lexing error: " ^ msg)
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

let parse_signature (s : string) : typed_var list =
   let lexbuf = Sedlexing.Utf8.from_string s in
   let lexer = Sedlexing.with_tokenizer C_lexer.token lexbuf in
   let parser =
     MenhirLib.Convert.Simplified.traditional2revised C_parser.signature in
   try
     parser lexer
   with
   | C_lexer.Lexing_error  (_pos, _col, msg) ->
       failwith ("Lexing error: " ^ msg)
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

let is_proto (p : parse_result) = match p with
    | Prototype _ -> true
    | _ -> false

let is_typedef (p : parse_result) = match p with
    | Typedef _ -> true
    | _ -> false

let to_ctypes (p : parse_result) : (string * ctype * (typed_var list)) option = match p with
    | Prototype (name, ret, signature) -> 
        (try
            let ctype_sig = parse_signature signature in
            let ctype_ret = parse_ret_type ret in Some (name, ctype_ret, ctype_sig)
        with  
        | _-> None)
    | _ -> failwith ""

let string_of_ctypes ((name, ret, signature) : string * ctype * (typed_var list)) = 
    Printf.sprintf "%s : (%s) -> %s" name (show_ctype ret) (List.map show_typed_var signature |> String.concat " , ")

let get_typedef p = match p with
  | Typedef s -> s
  | _ -> failwith ""

let is_spec_type s = match s with
  | CBaseSort _ -> true
  | _ -> false 

let is_pointer_level s = match s with
  | PtrLevel _ -> true
  | _ -> false 

let get_type_from_spec_list sl = 
  let sorts = List.filter is_spec_type sl in 
  let len = List.length sorts in
  if len = 1 then 
    List.hd sorts
else if len = 0 then 
  (* default to int *)
  CBaseSort "int"
else failwith "Illegal type - more than one base type"

let get_pointer_level sl = 
  let levels = List.filter is_pointer_level sl in
  let len = List.length levels in
  if len = 1 then 
      match List.hd levels with
        | PtrLevel x -> x
        | _ -> failwith ""
  else if len = 0 then 0
  else failwith "Illegal type - multiple pointer levels"

let perktype_of_sort s : perktype = match s with
  | CBaseSort x -> ([], Basetype x, [])
  | _ -> failwith ""

(* TODO preserve qualifiers and specifiers of pointer *)
let rec wrap_perktype_in_ptrs n t = 
  if n = 0 then t
  else [], Pointertype (wrap_perktype_in_ptrs (n-1) t), []

(* TODO add qualifiers and solve specifiers like long long, unsigned long .... to uint64_t et al *)
let perktype_of_ctype c = match c with
  | CBaseType specifiers ->(
    let type_spec = get_type_from_spec_list specifiers in 
    let pointer_level = get_pointer_level specifiers in
    perktype_of_sort type_spec |> wrap_perktype_in_ptrs pointer_level
  )

let perkvardesc_of_arg (tv) = match tv with
  | TypedVar (t, IdenDecl _name) -> perktype_of_ctype t
  | Ellipsis -> ([], Vararg, [])

let get_prototype_types () : (string * perktype) list = 
    let lines = read_file "tags" in
    let parsed_lines = List.map parse_line lines in
    let typedef_vars = parsed_lines |> List.filter is_typedef |> List.map get_typedef in
    let proto_types = 
      C_lexer.type_names := !C_lexer.type_names @ typedef_vars;
      parsed_lines |> List.filter is_proto |> List.map to_ctypes |> List.filter Option.is_some |> List.map Option.get in

    let proto_type_to_funtype (name, ctyp, args) = 
      let ret_type = perktype_of_ctype ctyp in
      let arg_types = List.map perkvardesc_of_arg args in
      name, ([], Funtype (arg_types, ret_type), [])
    in
    List.map proto_type_to_funtype proto_types
    
