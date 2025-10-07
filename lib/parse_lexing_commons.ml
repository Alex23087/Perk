let constructor_names : string list ref = ref []

let add_constructor_name n =
  if not (List.mem n !constructor_names) then
    constructor_names := n :: !constructor_names

let is_constructor_name n = List.mem n !constructor_names

let print_constructor_names () =
  Printf.printf "constructors:\n%s\n"
    (String.concat "\n"
       (List.map (fun x -> "    \"" ^ x ^ "\"") !constructor_names))
