open Perk.Compile_utils
open Cmdliner

(* Command line arguments *)
let input_file =
  let doc = "Input Perk source file to compile" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"IN_FILE" ~doc)

let check_only =
  let doc = "Only check the file for syntax and type errors, don't compile" in
  Arg.(value & flag & info [ "c"; "check" ] ~doc)

let static_compilation =
  let doc = "Compile in static mode" in
  Arg.(value & flag & info [ "s"; "static" ] ~doc)

let verbose =
  let doc = "Enable verbose output" in
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc)

let output_file =
  let doc = "Output file name (default: derived from input file)" in
  Arg.(
    value
    & opt (some string) None
    & info [ "o"; "output" ] ~docv:"OUT_FILE" ~doc)

let dir =
  let doc = "Behave as if the input file were in this directory" in
  Arg.(value & opt (some string) None & info [ "d"; "dir" ] ~docv:"DIR" ~doc)

let c_compiler =
  let doc = "C compiler to use (default: gcc)" in
  Arg.(value & opt string "gcc" & info [ "cc"; "c-compiler" ] ~docv:"CC" ~doc)

let c_flags =
  let doc = "Additional flags to pass to the C compiler" in
  Arg.(value & opt string "" & info [ "cflags" ] ~docv:"CFLAGS" ~doc)

(* Main command implementation *)
let perkc_cmd check_only static_compilation verbose output_file input_file
    (dir : string option) (c_compiler : string) (c_flags : string) =
  if verbose then Printf.printf "Processing file: %s\n" input_file;

  if check_only then (
    if verbose then Printf.printf "Running syntax and type check only\n";
    ignore
      (check_file ?dir static_compilation verbose input_file c_compiler c_flags);
    `Ok ())
  else (
    if verbose then Printf.printf "Compiling to C\n";
    compile_program ?dir static_compilation verbose input_file output_file
      c_compiler c_flags;
    `Ok ())

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
  let info = Cmd.info "perkc" ~version:"1.0" ~doc ~man in
  Cmd.v info
    Term.(
      ret
        (const perkc_cmd $ check_only $ static_compilation $ verbose
       $ output_file $ input_file $ dir $ c_compiler $ c_flags))

let () = exit (Cmd.eval cmd)
