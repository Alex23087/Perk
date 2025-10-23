open Ast
open Codegen
open Errors (*EWWOWS*)
open Typecheck

let opens_hashmap : (string, unit) Hashtbl.t = Hashtbl.create 16

let add_import (import : string) : bool =
  let import = Fpath.to_string (Fpath.normalize (Fpath.v import)) in
  Printf.printf "%s" (Printf.sprintf "Adding import to hashmap: %s\n" import);
  if Hashtbl.mem opens_hashmap import then false
  else (
    Hashtbl.add opens_hashmap import ();
    true)

let ast_of_filename filename =
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
  ast

let singletonamble =
  if !Utils.static_compilation then ""
  else
    "#include <malloc.h>\n#include <string.h>\n#include <stdbool.h>\n"
    ^ "#ifndef LAMBDUMMY_PERK\n#define LAMBDUMMY_PERK\n"
    ^ "typedef struct _lambdummy_type {\n\
      \    void *env;\n\
      \    void *func;\n\
       } __lambdummy_type;\n\
       static __lambdummy_type *__lambdummy;\n\n\
      \ __lambdummy_type *alloclabmd(int size, void *labmda, void *env)\n\
       {\n\
      \    __lambdummy_type *ptr = malloc(sizeof(__lambdummy_type));\n\
      \    ptr->env = malloc(size);\n\
      \    memcpy(ptr->env, env, size);\n\
      \    ptr->func = labmda;\n\
      \    return ptr;\n\
       }" ^ "\n\n"
    ^ "#define CALL_LAMBDA0(l, t) (__lambdummy = (__lambdummy_type *)l, \
       ((t)(__lambdummy->func))())\n\
       #define CALL_LAMBDA(l, t, ...) (__lambdummy = (__lambdummy_type \
       *)l,       ((t)(__lambdummy->func))(__VA_ARGS__))" ^ "\n\n" ^ "#endif\n"

let rec compile_program ?(dir : string option) ?(dry_run = false)
    ?(json_format = false) (static_compilation : bool) (verbose : bool)
    (input_file : string) (output_file : string option) (c_compiler : string)
    (c_flags : string) =
  (* let out_ast_file = Filename.chop_suffix input_file ".perk" ^ ".ast" in *)
  Utils.static_compilation := static_compilation;
  Utils.verbose := verbose;
  Utils.c_compiler := c_compiler;
  Utils.c_flags := c_flags;
  try
    let _ast, (compiled_preamble, compiled_body) =
      process_file ?dir input_file true
    in
    if not dry_run then (
      let out_file_c =
        if Option.is_some output_file then Option.get output_file
        else Filename.chop_suffix input_file ".perk" ^ ".c"
      in

      let out_file_h =
        if Option.is_some output_file then Option.get output_file
        else Filename.chop_suffix input_file ".perk" ^ ".h"
      in
      let lambdummy_h =
        Filename.concat (Filename.dirname input_file) "perklang.h"
      in
      (* let oaf = open_out out_ast_file in
    output_string oaf ast; *)
      let oc = open_out out_file_c in
      output_string oc ("#include \"perklang.h\"\n" ^ compiled_body);
      close_out oc;
      let oc = open_out out_file_h in
      output_string oc compiled_preamble;
      close_out oc;
      let oc = open_out lambdummy_h in
      output_string oc singletonamble;
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

and process_file ?(dir : string option) (filename : string) (is_main : bool) :
    string * (string * string) =
  let filename =
    (* If a directory override is provided, behave as if the file were in that directory *)
    if Option.is_some dir then
      Filename.concat (Option.get dir) (Filename.basename filename)
    else filename
  in
  let filename_canonical =
    Fpath.to_string (Fpath.normalize (Fpath.v filename))
  in
  (* Get base name here because generated .h is in the same directory as the .c *)
  let header_name =
    Filename.chop_suffix (Filename.basename filename_canonical) ".perk" ^ ".h"
  in
  Printf.printf "%s"
    (Printf.sprintf "Processing file: %s (canonical: %s)\n" filename
       filename_canonical);
  add_import filename_canonical |> ignore;
  let dirname =
    if dir = None then Filename.dirname filename else Option.get dir
  in
  let old_fnm = !Utils.fnm in
  Utils.fnm := filename;
  let ast = ast_of_filename filename in
  let ast = opens_to_imports dirname ast in
  let ast = remove_opens ast in
  let ast = typecheck_program ast in
  let out =
    ( String.concat "\n" (List.map show_topleveldef_a ast),
      ast |> codegen_program header_name is_main )
  in
  if old_fnm <> "" then Utils.fnm := old_fnm;
  out

and opens_to_imports (dir : string) (ast : topleveldef_a list) :
    topleveldef_a list =
  (* this is pretty fucking stupid -- does not work with .. *)
  (* let rec remove_dir_from_path path dir =
    let aux path dir =
      let parts = String.split_on_char '/' path in
      match parts with
      | [] -> path
      | first :: rest -> if first = dir then String.concat "/" rest else path
    in
    (* Printf.printf "remove %s from %s\n" dir path; *)
    let path =
      if String.starts_with ~prefix:"./" path then
        remove_dir_from_path (aux path ".") "."
      else path
    in
    let dir =
      if String.starts_with ~prefix:"./" dir then
        remove_dir_from_path (aux dir ".") "."
      else dir
    in
    aux path dir
  in *)
  let remove_dir_from_path i _dir = i in
  let import_paths = process_opens dir ast in
  let imports =
    List.map
      (fun (i, n) ->
        Printf.printf "import file: %s, dir: %s, fnm: %s\n" i dir !Utils.fnm;

        annot_copy n
          (* PROBLEM: the file i has path local to the source directory... *)
          (Import
             ("\""
             ^ (Filename.chop_suffix (remove_dir_from_path i dir) ".perk" ^ ".h")
             ^ "\"")))
      import_paths
  in
  imports @ ast

and remove_opens ast : topleveldef_a list =
  match ast with
  | { loc = _; node = Open _ } :: rest -> remove_opens rest
  | x :: rest -> x :: remove_opens rest
  | [] -> []

(** @return original path as written in the file, and the Open ast node *)
and process_opens (dir : string) (ast : topleveldef_a list) :
    (string * topleveldef_a) list =
  match ast with
  | ({ loc = _; node = Open i } as node) :: rest ->
      let open_filename =
        if Fpath.is_rel (Fpath.v i) then
          Fpath.(to_string (normalize (v dir // v i)))
        else Fpath.(to_string (normalize (v i)))
      in
      (* let open_filename = i in *)
      let did_add = add_import open_filename in
      if not (Sys.file_exists open_filename) then
        raise_compilation_error node
          (Printf.sprintf "File %s does not exist" open_filename);
      if did_add then (
        let _ast, (compiled_preamble, compiled_body) =
          Printf.printf "%s"
            (Printf.sprintf "Processing open file: %s\n" open_filename);
          process_file open_filename false
        in
        let out_file_c = Filename.chop_suffix open_filename ".perk" ^ ".c" in
        let out_file_h = Filename.chop_suffix open_filename ".perk" ^ ".h" in
        let oc = open_out out_file_c in
        Printf.printf "%s" (Printf.sprintf "saving file %s\n" out_file_c);
        output_string oc compiled_body;
        close_out oc;
        let oc = open_out out_file_h in
        Printf.printf "%s" (Printf.sprintf "saving file %s\n" out_file_h);
        output_string oc compiled_preamble;
        close_out oc;
        [ (i, node) ] @ process_opens dir rest)
      else process_opens dir rest
  | _ :: rest -> process_opens dir rest
  | [] -> []

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
