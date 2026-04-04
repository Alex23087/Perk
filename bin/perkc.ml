open Perk.Compile_utils
open Cmdliner

(* Command line arguments *)
let input_file =
  let doc = "Input Perk source file to compile" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"IN_FILE" ~doc)

let check_only =
  let doc = "Only check the file for syntax and type errors, don't compile" in
  Arg.(value & flag & info [ "c"; "check"; "dry-run" ] ~doc)

let json_format =
  let doc = "Output errors and warnings in JSON format" in
  Arg.(value & flag & info [ "j"; "json" ] ~doc)

let static_compilation =
  let doc = "Compile in static mode" in
  Arg.(value & flag & info [ "s"; "static" ] ~doc)

let verbose =
  let doc = "Enable verbose output" in
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc)

let record_stack_trace =
  let doc = "Enable stack trace recording (for debugs purposes only)" in
  Arg.(value & flag & info [ "rst"; "record-stack-trace" ] ~doc)

let retain_tmp_files =
  let doc = "Retain temporary files generated during compilation" in
  Arg.(value & flag & info [ "rtf"; "retain-temp-files" ] ~doc)

let output_dir =
  let doc = "Output directory name (default: \"\")" in
  Arg.(
    value & opt (some string) None & info [ "o"; "output" ] ~docv:"OUT_DIR" ~doc)

let dir =
  let doc = "Behave as if the input file were in this directory" in
  Arg.(value & opt (some string) None & info [ "d"; "dir" ] ~docv:"DIR" ~doc)

let c_compiler =
  let doc = "C compiler to use (default: gcc)" in
  Arg.(value & opt string "gcc" & info [ "cc"; "c-compiler" ] ~docv:"CC" ~doc)

let c_flags =
  let doc = "Additional flags to pass to the C compiler" in
  Arg.(value & opt string "" & info [ "cflags" ] ~docv:"CFLAGS" ~doc)

let include_paths =
  let doc = "Add DIR to the include search path (repeatable)" in
  Arg.(value & opt_all string [] & info [ "I"; "include-dir" ] ~docv:"DIR" ~doc)

(* Main command implementation *)
let perkc_cmd check_only json_format static_compilation verbose
    (record_stack_trace : bool) (retain_tmp_files : bool) output_dir input_file
    (dir : string option) (c_compiler : string) (c_flags : string)
    (include_paths : string list) =
  let base_dir =
    let cwd = Sys.getcwd () in
    match dir with
    | Some provided ->
        let absolute =
          if Filename.is_relative provided then Filename.concat cwd provided
          else provided
        in
        Fpath.(absolute |> v |> normalize |> to_string)
    | None -> Fpath.(cwd |> v |> normalize |> to_string)
  in
  let normalize_include_path path =
    let absolute =
      if Filename.is_relative path then Filename.concat base_dir path else path
    in
    Fpath.(absolute |> v |> normalize |> to_string)
  in
  let normalized_include_paths =
    List.map normalize_include_path include_paths
  in
  Perk.Utils.include_paths :=
    !Perk.Utils.include_paths @ normalized_include_paths;
  let include_flag_string =
    normalized_include_paths
    |> List.map (Printf.sprintf "-I%s ")
    |> String.concat " "
  in
  let combined_c_flags =
    String.concat " "
      (List.filter (fun s -> s <> "") [ c_flags; include_flag_string ])
  in
  if verbose then (
    Printf.printf "Processing file: %s\n" input_file;
    Printf.printf "Running in %s mode\n"
      (if static_compilation then "static" else "hosted");
    Perk.Utils.verbose := true);

  if check_only then (
    if verbose then Printf.printf "Running syntax and type check only\n")
  else if verbose then Printf.printf "Compiling to C\n";
  compile_program ?dir ?dry_run:(Some check_only)
    ?json_format:(Some json_format) static_compilation verbose
    record_stack_trace retain_tmp_files input_file output_dir c_compiler
    combined_c_flags;
  `Ok ()

(* Command definition *)
let cmd =
  let doc = "Perk compiler - compile Perk source files to C" in
  let man =
    [
      `S Manpage.s_description;
      `P "$(tname) compiles Perk source files to C code.";
      `P
        "Use --check to only validate syntax and types without generating \
         output.";
      `S Manpage.s_examples;
      `P "Compile a file:";
      `P "  $(mname) program.perk";
      `P "Check syntax only:";
      `P "  $(mname) --check program.perk";
      `S Manpage.s_bugs;
      `P "Report bugs to https://github.com/Alex23087/Perk/issues";
    ]
  in
  let info = Cmd.info "perkc" ~version:"0.89.0" ~doc ~man in
  Cmd.v info
    Term.(
      ret
        (const perkc_cmd $ check_only $ json_format $ static_compilation
       $ verbose $ record_stack_trace $ retain_tmp_files $ output_dir
       $ input_file $ dir $ c_compiler $ c_flags $ include_paths))

let () = exit (Cmd.eval cmd)
