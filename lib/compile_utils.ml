open Ast
open Codegen
open Errors (*EWWOWS*)
open Typecheck

let opens_hashmap : (string, unit) Hashtbl.t = Hashtbl.create 16

let add_import (import : string) : bool =
  if Hashtbl.mem opens_hashmap import then false
  else (
    Hashtbl.add opens_hashmap import ();
    true)

let ast_of_filename filename =
  let old_fnm = !Utils.fnm in
  Utils.fnm := filename;
  let inchn = open_in filename in
  let ast_of_channel inchn =
    let lexbuf = Sedlexing.Utf8.from_channel inchn in
    Sedlexing.set_filename lexbuf filename;
    let lexer = Sedlexing.with_tokenizer Lexer.token lexbuf in
    let parser =
      MenhirLib.Convert.Simplified.traditional2revised Parser.program
    in
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
    | Parser.Error ->
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
  if old_fnm <> "" then Utils.fnm := old_fnm;
  ast

let rec compile_program ?(dir : string option) ?(dry_run = false)
    ?(json_format = false) (static_compilation : bool) (verbose : bool)
    (input_file : string) (output_file : string option) (c_compiler : string)
    (c_flags : string) =
  (* let out_ast_file = Filename.chop_suffix input_file ".perk" ^ ".ast" in *)
  try
    let _ast, compiled = process_file ?dir input_file in
    if not dry_run then (
      Utils.static_compilation := static_compilation;
      Utils.verbose := verbose;
      Utils.c_compiler := c_compiler;
      Utils.c_flags := c_flags;
      let out_file =
        if Option.is_some output_file then Option.get output_file
        else Filename.chop_suffix input_file ".perk" ^ ".c"
      in

      (* let oaf = open_out out_ast_file in
    output_string oaf ast; *)
      let oc = open_out out_file in
      output_string oc compiled;
      close_out oc)
  with
  | Syntax_error ((start_line, start_col), (end_line, end_col), input_file, msg)
    ->
      if json_format then (
        Printf.printf
          "{\"error\": \"syntax\", \"start_line\": %d, \"start_col\": %d, \
           \"end_line\": %d, \"end_col\": %d, \"message\": \"%s\", \"file\": \
           \"%s\"}\n"
          start_line start_col end_line end_col (String.escaped msg) input_file;
        exit 0)
      else
        Printf.eprintf
          "\027[31mSyntax error at line %d, column %d: %s, ending at line %d, \
           column %d in file %s\027[0m\n"
          start_line start_col msg end_line end_col input_file;
      exit 1
  | Lexing_error ((start_line, start_col), (end_line, end_col), input_file, msg)
    ->
      if json_format then (
        Printf.printf
          "{\"error\": \"lexing\", \"start_line\": %d, \"start_col\": %d, \
           \"end_line\": %d, \"end_col\": %d, \"message\": \"%s\", \"file\": \
           \"%s\"}\n"
          start_line start_col end_line end_col (String.escaped msg) input_file;
        exit 0)
      else
        Printf.eprintf
          "\027[31mLexing error at line %d, column %d: %s, ending at line %d, \
           column %d in file %s\027[0m\n"
          start_line start_col msg end_line end_col input_file;
      exit 1
  | Type_error ((start_line, start_col), (end_line, end_col), input_file, msg)
    ->
      if json_format then (
        Printf.printf
          "{\"error\": \"typecheck\", \"start_line\": %d, \"start_col\": %d, \
           \"end_line\": %d, \"end_col\": %d, \"message\": \"%s\", \"file\": \
           \"%s\"}\n"
          start_line start_col end_line end_col (String.escaped msg) input_file;
        exit 0)
      else
        Printf.eprintf
          "\027[31mType error at line %d, column %d: %s, ending at line %d, \
           column %d in file %s\027[0m\n"
          start_line start_col msg end_line end_col input_file;
      exit 1
  | Parser.Error ->
      if json_format then (
        Printf.printf
          "{\"error\": \"parse\", \"message\": \"Unexpected token in file %s\"}\n"
          !Utils.fnm;
        exit 0)
      else
        Printf.eprintf
          "\027[31mParsing error: unexpected token in file %s\027[0m\n"
          input_file;
      exit 1
  | Compilation_error
      ((start_line, start_col), (end_line, end_col), input_file, msg) ->
      if json_format then (
        Printf.printf
          "{\"error\": \"compilation\", \"start_line\": %d, \"start_col\": %d, \
           \"end_line\": %d, \"end_col\": %d, \"message\": \"%s\", \"file\": \
           \"%s\"}\n"
          start_line start_col end_line end_col (String.escaped msg) input_file;
        exit 0)
      else
        Printf.eprintf
          "\027[31mCompilation error at line %d, column %d: %s, ending at line \
           %d, column %d in file %s\027[0m\n"
          start_line start_col msg end_line end_col input_file;
      exit 1

and process_file ?(dir : string option) (filename : string) : string * string =
  let filename =
    (* If a directory override is provided, behave as if the file were in that directory *)
    if Option.is_some dir then
      Filename.concat (Option.get dir) (Filename.basename filename)
    else filename
  in
  let filename_canonical =
    Fpath.to_string (Fpath.normalize (Fpath.v filename))
  in
  add_import filename_canonical |> ignore;
  let dirname =
    if dir = None then Filename.dirname filename else Option.get dir
  in
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
  | ({ loc = _; node = Open i } as node) :: rest ->
      let open_filename =
        if Fpath.is_rel (Fpath.v i) then
          Fpath.(to_string (normalize (v dir // v i)))
        else Fpath.(to_string (normalize (v i)))
      in
      let did_add = add_import open_filename in
      if not (Sys.file_exists open_filename) then
        raise_compilation_error node
          (Printf.sprintf "File %s does not exist" open_filename);
      if did_add then
        expand_opens
          (Filename.dirname open_filename)
          (ast_of_filename open_filename)
        @ expand_opens dir rest
      else expand_opens dir rest
  (* | ({ loc = _; node = Import i } as node) :: rest ->
      if String.starts_with ~prefix:"\"" i then (
        let i = String.sub i 1 (String.length i - 2) in
        let import_filename =
          if Fpath.is_rel (Fpath.v i) then
            Fpath.(to_string (normalize (v dir // v i)))
          else Fpath.(to_string (normalize (v i)))
        in
        let did_add = add_import import_filename in
        if not (Sys.file_exists import_filename) then
          raise_compilation_error node
            (Printf.sprintf "File %s does not exist" import_filename);
        if did_add then
          let import_filename = "\"" ^ import_filename ^ "\"" in
          annot_copy node (Import import_filename) :: expand_opens dir rest
        else expand_opens dir rest)
      else node :: expand_opens dir rest *)
  | x :: rest -> x :: expand_opens dir rest
  | [] -> []

and check_file ?dir (static_compilation : bool) (verbose : bool)
    (filename : string) (c_compiler : string) (c_flags : string) : unit =
  Utils.static_compilation := static_compilation;
  Utils.verbose := verbose;
  Utils.c_compiler := c_compiler;
  Utils.c_flags := c_flags;
  try process_file ?dir filename |> ignore with
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
  | Parser.Error ->
      Printf.printf
        "{\"error\": \"parse\", \"message\": \"Unexpected token in file %s\"}\n"
        !Utils.fnm;
      exit 0
  | Compilation_error
      ((start_line, start_col), (end_line, end_col), input_file, msg) ->
      Printf.printf
        "{\"error\": \"compilation\", \"start_line\": %d, \"start_col\": %d, \
         \"end_line\": %d, \"end_col\": %d, \"message\": \"%s\", \"file\": \
         \"%s\"}\n"
        start_line start_col end_line end_col (String.escaped msg) input_file;
      exit 0
