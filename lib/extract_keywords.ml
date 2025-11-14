(** Extract reserved keywords from lexer.ml automatically.
    This parses the lexer to find all string patterns that produce keyword tokens
    (non-Ident, non-operator tokens). Run at build time. *)

let extract_keywords filename =
  let ic = open_in filename in
  let keywords = ref [] in
  let in_token_function = ref false in
  
  let rec loop () =
    try
      let line = input_line ic in
      let trimmed = String.trim line in
      
      (* Detect when we're inside the token function *)
      if String.length trimmed > 10 && 
         String.sub trimmed 0 10 = "let rec to" then
        in_token_function := true;
      
      (* Stop when we exit the token function *)
      if !in_token_function && String.starts_with ~prefix:"and " trimmed then
        in_token_function := false;
      
      (* Extract keyword patterns like: | "keyword" -> Token *)
      if !in_token_function && String.contains trimmed '|' then begin
        (* Match patterns: | "word" -> TokenName (or | "word1" | "word2" -> ...) *)
        let parts = String.split_on_char '|' line in
        List.iter (fun part ->
          let part = String.trim part in
          (* Look for quoted strings before -> *)
          if String.contains part '"' && String.contains part '-' then begin
            let before_arrow = 
              try
                let arrow_pos = String.index part '-' in
                String.sub part 0 arrow_pos
              with Not_found -> part
            in
            (* Extract all quoted strings *)
            let rec extract_quoted str pos acc =
              try
                let start = String.index_from str pos '"' in
                let end_pos = String.index_from str (start + 1) '"' in
                let keyword = String.sub str (start + 1) (end_pos - start - 1) in
                (* Only keep alphabetic keywords (identifiers), not operators *)
                if String.length keyword > 0 && 
                   keyword.[0] >= 'a' && keyword.[0] <= 'z' then
                  extract_quoted str (end_pos + 1) (keyword :: acc)
                else
                  extract_quoted str (end_pos + 1) acc
              with Not_found -> acc
            in
            let found = extract_quoted before_arrow 0 [] in
            keywords := !keywords @ found
          end
        ) parts
      end;
      
      loop ()
    with End_of_file ->
      close_in ic
  in
  
  loop ();
  List.sort_uniq String.compare !keywords

let () =
  if Array.length Sys.argv < 3 then begin
    Printf.eprintf "Usage: %s <lexer.ml> <output.ml>\n" Sys.argv.(0);
    exit 1
  end;
  
  let lexer_file = Sys.argv.(1) in
  let output_file = Sys.argv.(2) in
  let keywords = extract_keywords lexer_file in
  
  (* Generate keyword_list.ml *)
  let oc = open_out output_file in
  Printf.fprintf oc "(* AUTO-GENERATED from lexer.ml - DO NOT EDIT MANUALLY *)\n\n";
  Printf.fprintf oc "let reserved_keywords = [\n";
  List.iter (fun kw ->
    Printf.fprintf oc "  %S;\n" kw
  ) keywords;
  Printf.fprintf oc "]\n";
  close_out oc;
  
  Printf.printf "Generated %s with %d keywords\n" output_file (List.length keywords)
