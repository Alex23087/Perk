open Perkelang.Compile_utils

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
