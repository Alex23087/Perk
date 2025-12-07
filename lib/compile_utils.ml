open Ast
open Codegen
open Errors (*EWWOWS*)
open Error_codes
open Typecheck
open File_info

let opens_hashmap : (string, unit) Hashtbl.t = Hashtbl.create 16

let add_import (import : string) : bool =
  let import = Fpath.to_string (Fpath.normalize (Fpath.v import)) in
  if Hashtbl.mem opens_hashmap import then false
  else (
    if !Utils.verbose then (
      Utils.say_here (Printf.sprintf "Adding import to hashmap: %s" import);
      Utils.say_here "Open hashmap entries:";
      Hashtbl.iter
        (fun key _ -> Printf.sprintf "%s" key |> Utils.say_here)
        opens_hashmap);
    Hashtbl.add opens_hashmap import ();
    true)

let run_and_capture cmd =
  let tmp = Filename.temp_file "ocaml_cmd" ".txt" in
  let full_cmd = cmd ^ " > " ^ tmp in
  let _ = Sys.command full_cmd in
  let ic = open_in tmp in
  let len = in_channel_length ic in
  let result = really_input_string ic len in
  close_in ic;
  Sys.remove tmp;
  result

let gather_numerical_lengths () : unit =
  let res = run_and_capture "echo | gcc -dM -E - | grep SIZEOF" in
  res |> String.split_on_char '\n'
  |> List.filter (fun line -> String.trim line <> "")
  |> List.iter (fun line ->
      match String.split_on_char ' ' (String.trim line) with
      | [ def; macro; value ] when String.starts_with ~prefix:"#define" def ->
          let key = macro in
          let value = int_of_string value in
          Utils.say_here (Printf.sprintf "Added size type: %s %d" key value);
          Hashtbl.add Utils.numerical_sizes key value
      | _ as s ->
          failwith
            ("Unexpected format in numerical lengths: " ^ String.concat " " s))

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
    | ParseError (f, e, code) ->
        raise
          (Syntax_error
             ( ( (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum,
                 (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
                 - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol ),
               ( (snd (Sedlexing.lexing_positions lexbuf)).pos_lnum,
                 (snd (Sedlexing.lexing_positions lexbuf)).pos_cnum
                 - (snd (Sedlexing.lexing_positions lexbuf)).pos_bol ),
               f,
               e,
               code ))
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
                an issue on https://github.com/Alex23087/Perk/issues",
               Unknown_error ))
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

let error_out json_format error_class start_line start_col end_line end_col msg
    file code exn record_stack_trace =
  Parse_tags.remove_libs_expanded ();
  Parse_tags.remove_tags ();
  if record_stack_trace then
    Printf.eprintf "Exception: %s\n%s\n" (Printexc.to_string exn)
      (Printexc.get_backtrace ());
  if json_format then (
    Printf.printf
      "{\"error\": \"%s\", \"start_line\": %d, \"start_col\": %d, \
       \"end_line\": %d, \"end_col\": %d, \"message\": \"%s\", \"file\": \
       \"%s\", \"error_code\": {\"description\": \"%s\", \"code\": %d}}\n"
      (fst error_class) start_line start_col end_line end_col
      (String.escaped msg) file (show_error_code code) (error_code_to_enum code);
    exit 0)
  else
    Printf.eprintf
      "\027[31m%s error at line %d, column %d: %s, ending at line %d, column \
       %d in file %s\027[0m\n"
      (snd error_class) start_line start_col msg end_line end_col file;
  exit 1

let rec compile_program ?(dir : string option) ?(dry_run = false)
    ?(json_format = false) (static_compilation : bool) (verbose : bool)
    (record_stack_trace : bool) (retain_tmp_files : bool) (input_file : string)
    (output_dir : string option) (c_compiler : string) (c_flags : string) =
  (* let out_ast_file = Filename.chop_suffix input_file ".perk" ^ ".ast" in *)
  if record_stack_trace then Printexc.record_backtrace true;
  gather_numerical_lengths ();

  Utils.static_compilation := static_compilation;
  Utils.verbose := verbose;
  Utils.retain_tmp_files := retain_tmp_files;
  Utils.c_compiler := c_compiler;
  Utils.c_flags := c_flags;

  Utils.target_dir_name :=
    if Option.is_some output_dir then Option.get output_dir else "";

  try
    let _ast, (compiled_preamble, compiled_body) =
      process_file ?dir input_file true
    in
    if not dry_run then (
      let out_file_c =
        (* if Option.is_some output_file then Option.get output_file else *)
        Filename.concat !Utils.target_dir_name
          (Filename.chop_suffix input_file ".perk" ^ ".c")
      in

      let out_file_h =
        (* if Option.is_some output_file then Option.get output_file else *)
        Filename.concat !Utils.target_dir_name
          (Filename.chop_suffix input_file ".perk" ^ ".h")
      in
      let lambdummy_h =
        Filename.concat !Utils.target_dir_name
          (Filename.concat (Filename.dirname input_file) "perklang.h")
      in

      Utils.create_file_with_dirs out_file_c;
      Utils.create_file_with_dirs out_file_h;
      Utils.create_file_with_dirs lambdummy_h;

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
  | Syntax_error
      ((start_line, start_col), (end_line, end_col), input_file, msg, code) as
    exn ->
      error_out json_format ("syntax", "Syntax") start_line start_col end_line
        end_col msg input_file code exn record_stack_trace
  | Lexing_error
      ((start_line, start_col), (end_line, end_col), input_file, msg, code) as
    exn ->
      error_out json_format ("lexing", "Lexing") start_line start_col end_line
        end_col msg input_file code exn record_stack_trace
  | Type_error
      ((start_line, start_col), (end_line, end_col), input_file, msg, code) as
    exn ->
      error_out json_format ("typecheck", "Type") start_line start_col end_line
        end_col msg input_file code exn record_stack_trace
  | Parser.Error as exn ->
      error_out json_format ("parse", "parsing") (-1) (-1) (-1) (-1)
        "Unexpected token" input_file Unknown_error exn record_stack_trace
  | Compilation_error
      ((start_line, start_col), (end_line, end_col), input_file, msg, code) as
    exn ->
      error_out json_format
        ("compilation", "Compilation")
        start_line start_col end_line end_col msg input_file code exn
        record_stack_trace
  | exn ->
      error_out json_format ("internal", "Internal") (-1) (-1) (-1) (-1)
        (Printf.sprintf
           "Unhandled exception: \"%s\". If you see this error, please open an \
            issue at https://github.com/Alex23087/Perk/issues"
           (Printexc.to_string exn))
        input_file Internal_error exn record_stack_trace

(** Generates the global polyfun definitions that are not local to the current
    file *)
and add_polydefs ast =
  let definitions =
    List.fold_right
      (fun (tld, t_actual) acc ->
        annot_copy tld
          (match ( $ ) tld with
          | PolymorphicFundef ((t_res, id, args, body), t_param) ->
              if not (polyfun_is_already_codegened id t_actual) then (
                let param_types = List.map fst args in
                (* the definition is added to the file-local polyfun hashtable *)
                Hashtbl.add
                  (File_info.get_file_local_polyfuns ())
                  id
                  (param_types, t_res, t_param);

                (* Printf.printf
                  "function %s<%s> bout to be instantieted in add polydefs\n" id
                  (show_perktype t_actual); *)

                (* the polyfun instance is set as code-generated so it is not generated again *)
                set_polyfun_as_codegened id t_actual;
                let x =
                  Fundef
                    ( ( Polymorphism.subst_type t_res t_param t_actual,
                        id ^ "perk_polym_"
                        ^ Type_symbol_table.type_descriptor_of_perktype t_actual,
                        List.map
                          (fun x ->
                            Polymorphism.subst_perkvardesc x t_param t_actual)
                          args,
                        Polymorphism.subst_type_command body t_param t_actual ),
                      Normal,
                      false )
                in
                let t = typecheck_topleveldef (annot_copy tld x) in
                ( $ ) t)
              else
                (* Printf.printf "%s<%s> was already defined\n" id
                  (show_perktype t_actual); *)
                TLSkip
          | _ ->
              failwith
                "should not happen - non-polydef was passed to add_polydefs")
        :: acc)
      !(get_polyfuns_to_be_defined ())
      []
  in
  definitions @ ast

(** for each polydef, typedefs all of its instances. TODO check if this is
    necessary *)
and check_polydefs_pass (ast : topleveldef_a list) =
  List.map
    (fun tld ->
      match ( $ ) tld with
      | PolymorphicFundef ((t_res, id, args, body), t_param) ->
          let instances =
            try Hashtbl.find (File_info.get_polyfun_instances ()) id
            with Not_found -> []
          in

          List.map
            (fun (t_actual, _) ->
              let fundef =
                Fundef
                  ( ( Polymorphism.subst_type t_res t_param t_actual,
                      id ^ "perk_polym_"
                      ^ Type_symbol_table.type_descriptor_of_perktype t_actual,
                      List.map
                        (fun x ->
                          Polymorphism.subst_perkvardesc x t_param t_actual)
                        args,
                      Polymorphism.subst_type_command body t_param t_actual ),
                    Normal,
                    false )
              in
              let t = typecheck_topleveldef (annot_copy tld fundef) in
              t)
            instances
          |> ignore
      | _ -> ())
    ast
  |> ignore

and process_file ?(dir : string option) (filename : string) (is_main : bool) :
    string * (string * string) =
  (* creates new file info tables*)
  set_new_file_info ();
  let filename =
    (* If a directory override is provided, behave as if the file were in that directory *)
    if Option.is_some dir && not is_main then
      Filename.concat (Option.get dir) (Filename.basename filename)
    else filename
  in
  let filename_canonical =
    Fpath.to_string (Fpath.normalize (Fpath.v filename))
  in
  let dirname =
    if dir = None then Filename.dirname filename else Option.get dir
  in

  if not (!Utils.target_dir_name = "") then
    Utils.copy_non_perk_files dirname
      (Filename.concat !Utils.target_dir_name dirname);

  (* Get base name here because generated .h is in the same directory as the .c *)
  let header_name =
    Filename.chop_suffix (Filename.basename filename_canonical) ".perk" ^ ".h"
  in
  Utils.say_here
    (Printf.sprintf "Processing file: %s (canonical: %s)\n" filename
       filename_canonical);
  add_import filename_canonical |> ignore;

  let old_fnm = !Utils.fnm in
  Utils.fnm := filename;
  let ast = ast_of_filename filename in
  Var_symbol_table.push_symbol_table ();
  process_C_imports ast;
  let ast = opens_to_imports dirname ast in
  let ast = remove_opens ast in
  let ast = typecheck_program ast in
  let ast = add_polydefs ast in
  check_polydefs_pass ast;
  let out =
    ( String.concat "\n" (List.map show_topleveldef_a ast),
      ast |> codegen_program header_name is_main )
  in

  (* Utils.say_here "TYPECHECK IMPORTS:";
  List.iter
    (fun path -> Utils.say_here (Printf.sprintf "Import path: %s\n" path))
    !(File_info.get_import_paths ());
  Utils.say_here "FILE_INFO IMPORTS:";
  List.iter
    (fun path -> Utils.say_here (Printf.sprintf "Import path: %s\n" path))
    !(File_info.get_import_list ()); *)

  (* Hashtbl.clear Codegen.lambdas_hashmap;
  Hashtbl.clear Codegen.public_fundecl_symbol_table;
  Hashtbl.clear Codegen.private_fundecl_symbol_table;
  import_list := [];
  (* local polyfuns and polyfun instances are removed - only global polyfuns are preserved *)
  Hashtbl.clear Polymorphism.polyfun_instances;
  Hashtbl.clear Polymorphism.file_local_polyfuns;
  Polymorphism.polyfuns_to_be_defined := []; *)
  if old_fnm <> "" then Utils.fnm := old_fnm;
  out

and opens_to_imports (dir : string) (ast : topleveldef_a list) :
    topleveldef_a list =
  let import_paths = process_opens dir ast in
  let imports =
    List.map
      (fun (i, n) ->
        (* Printf.printf "import file: %s, dir: %s, fnm: %s, adj: %s\n" i dir
          !Utils.fnm i; *)
        annot_copy n
          (Import ("\"" ^ (Filename.chop_suffix i ".perk" (* *) ^ ".h") ^ "\"")))
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
      if did_add then (
        if not (Sys.file_exists open_filename) then
          raise_compilation_error node
            (Printf.sprintf "File %s does not exist" open_filename)
            File_not_found;
        let fi = save_file_info () in

        (* Store symbol table: the inner library shouldn't have access to the current symbol table,
          So we save it and reset it *)
        let symtable = ref !Var_symbol_table.var_symbol_table in
        Var_symbol_table.var_symbol_table := [];

        (* TODO: Type symbol table should be  *)
        let _ast, (compiled_preamble, compiled_body) =
          (* Printf.printf "%s"
            (Printf.sprintf "Processing open file: %s\n" open_filename); *)
          process_file open_filename false
        in

        (* Restore symbol table and add all global symbols defined in the opened library.
          Symbols included via C headers are allowed to be doubly declared. *)
        (* TODO: instead of blindly allowing C symbols to be doubly declared, there should be a finer check
          To ensure that they come from the same library as the symbol already declared (ideally checking if
          said library is also a #pragma once lib...) *)
        Var_symbol_table.append_symbol_table symtable
          ~filter:(fun (_, fnm) -> Utils.is_C_file fnm)
          (List.hd !Var_symbol_table.var_symbol_table);
        Var_symbol_table.var_symbol_table := !symtable;
        restore_file_info fi;

        (* TODO makeshift implementation, does not work for nested opens *)
        let out_file_c =
          Filename.concat !Utils.target_dir_name
            (Filename.chop_suffix open_filename ".perk" ^ ".c")
        in
        let out_file_h =
          Filename.concat !Utils.target_dir_name
            (Filename.chop_suffix open_filename ".perk" ^ ".h")
        in

        Utils.create_file_with_dirs out_file_c;
        Utils.create_file_with_dirs out_file_h;

        let oc = open_out out_file_c in
        (* Printf.printf "%s" (Printf.sprintf "saving file %s\n" out_file_c); *)
        output_string oc compiled_body;
        close_out oc;
        let oc = open_out out_file_h in
        (* Printf.printf "%s" (Printf.sprintf "saving file %s\n" out_file_h); *)
        output_string oc compiled_preamble;
        close_out oc;
        [ (i, node) ] @ process_opens dir rest)
      else process_opens dir rest
  | _ :: rest -> process_opens dir rest
  | [] -> []

and process_C_imports (ast : topleveldef_a list) =
  List.iter
    (fun tldf ->
      match ( $ ) tldf with
      | Import s ->
          File_info.get_import_paths ()
          := Typecheck.get_lib_path s :: !(File_info.get_import_paths ())
      | _ -> ())
    ast;
  if List.length !(File_info.get_import_paths ()) = 0 then ()
  else (
    (* Printf.printf "%d\n%s\n\n"
      (List.length !(File_info.get_import_paths ()))
      (String.concat ":" !(File_info.get_import_paths ())); *)
    Parse_tags.generate_tags !(File_info.get_import_paths ());
    Typecheck.library_functions := Parse_tags.get_prototype_types ();
    (* for each library function, if it is not already defined define it *)
    (* TODO solve conditionally compiled definitions *)
    (* TODO hoist these*)
    (* TODO: get filename of definition *)
    List.iter
      (fun (id, t) ->
        Utils.say_here
          (Printf.sprintf "Adding library function %s of type %s" id
             (show_perktype t));
        if Option.is_none (Var_symbol_table.lookup_var id) then
          Var_symbol_table.bind_var id ~fnm:"C_libs.h" t
        else Utils.say_here "Skipping")
      !Typecheck.library_functions;
    Parse_tags.remove_tags ();
    Parse_tags.remove_libs_expanded ())
