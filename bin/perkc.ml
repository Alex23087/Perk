open Perkelang.Ast
open Perkelang.Codegen
open Perkelang.Errors (*EWWOWS*)
open Perkelang.Typecheck

let opens_hashmap : (string, unit) Hashtbl.t = Hashtbl.create 16

let add_import (import : string) : bool =
  if Hashtbl.mem opens_hashmap import then false
  else (
    Hashtbl.add opens_hashmap import ();
    true)

let ast_of_filename filename =
  let inchn = open_in filename in
  let ast_of_channel inchn =
    let lexbuf = Sedlexing.Utf8.from_channel inchn in
    Sedlexing.set_filename lexbuf filename;
    let lexer = Sedlexing.with_tokenizer Perkelang.Lexer.token lexbuf in
    let parser =
      MenhirLib.Convert.Simplified.traditional2revised Perkelang.Parser.program
    in
    Perkelang.Utils.fnm := filename;
    try parser lexer with
    | ParseError (f, e) ->
        raise
          (Syntax_error
             ( ( (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum,
                 (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
                 - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol ),
               ( (snd (Sedlexing.lexing_positions lexbuf)).pos_lnum,
                 (snd (Sedlexing.lexing_positions lexbuf)).pos_cnum
                 - (snd (Sedlexing.lexing_positions lexbuf)).pos_bol ),
               f,
               e ))
    | Perkelang.Parser.Error ->
        raise
          (Syntax_error
             ( ( (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum,
                 (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
                 - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol ),
               ( (snd (Sedlexing.lexing_positions lexbuf)).pos_lnum,
                 (snd (Sedlexing.lexing_positions lexbuf)).pos_cnum
                 - (snd (Sedlexing.lexing_positions lexbuf)).pos_bol ),
               filename,
               "Unhandled parsing error. If this happens to you, please open \
                an issue on https://github.com/Alex23087/Perk/issues" ))
  in
  let ast = ast_of_channel inchn in
  close_in inchn;
  ast

let rec compile_program input_file =
  let out_file = Filename.chop_suffix input_file ".perk" ^ ".c" in

  (* let out_ast_file = Filename.chop_suffix input_file ".perk" ^ ".ast" in *)
  try
    let _ast, compiled = process_file input_file in

    (* let oaf = open_out out_ast_file in
    output_string oaf ast; *)
    let oc = open_out out_file in
    output_string oc compiled;
    close_out oc
  with
  | Syntax_error ((start_line, start_col), (end_line, end_col), input_file, msg)
    ->
      Printf.eprintf
        "\027[31mSyntax error at line %d, column %d: %s, ending at line %d, \
         column %d in file %s\027[0m\n"
        start_line start_col msg end_line end_col input_file;
      exit 1
  | Lexing_error ((start_line, start_col), (end_line, end_col), input_file, msg)
    ->
      Printf.eprintf
        "\027[31mLexing error at line %d, column %d: %s, ending at line %d, \
         column %d in file %s\027[0m\n"
        start_line start_col msg end_line end_col input_file;
      exit 1
  | Type_error ((start_line, start_col), (end_line, end_col), input_file, msg)
    ->
      Printf.eprintf
        "\027[31mType error at line %d, column %d: %s, ending at line %d, \
         column %d in file %s\027[0m\n"
        start_line start_col msg end_line end_col input_file;
      exit 1
  | Perkelang.Parser.Error ->
      Printf.eprintf
        "\027[31mParsing error: unexpected token in file %s\027[0m\n" input_file;
      exit 1

and process_file (filename : string) : string * string =
  let filename_canonical =
    Fpath.to_string (Fpath.normalize (Fpath.v filename))
  in
  add_import filename_canonical |> ignore;
  let dirname = Filename.dirname filename in
  let ast = ast_of_filename filename in
  let ast = expand_opens dirname ast in
  let ast = typecheck_program ast in
  let out =
    ( String.concat "\n" (List.map show_topleveldef_a ast),
      ast |> codegen_program )
  in
  out

and expand_opens (dir : string) (ast : topleveldef_a list) : topleveldef_a list
    =
  match ast with
  | { loc = _; node = Open i } :: rest ->
      let open_filename =
        if Fpath.is_rel (Fpath.v i) then
          Fpath.(to_string (normalize (v dir // v i)))
        else Fpath.(to_string (normalize (v i)))
      in
      let did_add = add_import open_filename in
      if did_add then
        expand_opens
          (Filename.dirname open_filename)
          (ast_of_filename open_filename @ rest)
      else expand_opens dir rest
  | x :: rest -> x :: expand_opens dir rest
  | [] -> []

and check_file (filename : string) : unit =
  try process_file filename |> ignore with
  | Syntax_error ((start_line, start_col), (end_line, end_col), input_file, msg)
    ->
      Printf.printf
        "{\"error\": \"syntax\", \"start_line\": %d, \"start_col\": %d, \
         \"end_line\": %d, \"end_col\": %d, \"message\": \"%s\", \"file\": \
         \"%s\"}\n"
        start_line start_col end_line end_col (String.escaped msg) input_file;
      exit 0
  | Lexing_error ((start_line, start_col), (end_line, end_col), input_file, msg)
    ->
      Printf.printf
        "{\"error\": \"lexing\", \"start_line\": %d, \"start_col\": %d, \
         \"end_line\": %d, \"end_col\": %d, \"message\": \"%s\", \"file\": \
         \"%s\"}\n"
        start_line start_col end_line end_col (String.escaped msg) input_file;
      exit 0
  | Type_error ((start_line, start_col), (end_line, end_col), input_file, msg)
    ->
      Printf.printf
        "{\"error\": \"typecheck\", \"start_line\": %d, \"start_col\": %d, \
         \"end_line\": %d, \"end_col\": %d, \"message\": \"%s\", \"file\": \
         \"%s\"}\n"
        start_line start_col end_line end_col (String.escaped msg) input_file;
      exit 0

let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "Usage: compiler <file.perk> or compiler --check";
    exit 1);

  let first_arg = Sys.argv.(1) in
  if first_arg = "--check" then (
    if Array.length Sys.argv < 3 then (
      prerr_endline "Usage: compiler --check <file.perk>";
      exit 1);
    let filename = Sys.argv.(2) in
    ignore (check_file filename);
    exit 0);

  compile_program first_arg
