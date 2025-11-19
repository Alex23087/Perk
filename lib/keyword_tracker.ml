(* Keyword tracking and validation for reserved word checking. *)
include Keyword_list

let last_keyword : string option ref = ref None
let last_keyword_clear () = last_keyword := None
let last_keyword_set (s : string) = last_keyword := Some s

let reserved_set = 
  let tbl = Hashtbl.create (List.length reserved_keywords) in
  List.iter (fun kw -> Hashtbl.add tbl kw ()) reserved_keywords;
  tbl

let is_reserved (s : string) : bool = Hashtbl.mem reserved_set s

let check_identifier (ctx : string) (id : string) : unit =
  if is_reserved id then
    raise (Errors.ParseError(!Utils.fnm, Printf.sprintf "keyword '%s' cannot be used as %s identifier" id ctx))

let validate_var_identifier = check_identifier "variable"
let validate_fun_identifier = check_identifier "function"
let validate_archetype_identifier = check_identifier "archetype"
let validate_type_identifier = check_identifier "type"
let validate_model_identifier = check_identifier "model"
let validate_struct_identifier = check_identifier "struct"

let raise_keyword_error (fnm : string ref) (ctx : string) =
  match !last_keyword with
  | Some kw when is_reserved kw -> raise (Errors.ParseError(!fnm, Printf.sprintf "keyword '%s' cannot be used as %s identifier" kw ctx))
  | _ -> raise (Errors.ParseError(!fnm, ctx ^ " expected"))
