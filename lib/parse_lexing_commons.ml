open Ast
open Type_symbol_table

let constructor_names : string list ref = ref []

let add_constructor_name n =
  if not (List.mem n !constructor_names) then
    constructor_names := n :: !constructor_names
;;

Utils.add_constructor_name_ptr := add_constructor_name

let is_constructor_name n = List.mem n !constructor_names

let print_constructor_names () =
  Printf.printf "constructors:\n%s\n"
    (String.concat "\n"
       (List.map (fun x -> "    \"" ^ x ^ "\"") !constructor_names))

let is_constructor_of (name : perkident) (typ : perktype) : bool =
  match typ |> resolve_type |> discard_type_aq with
  | AlgebraicType (_adt_name, constructors, _) ->
      List.exists (fun (ctor_name, _arg_types) -> ctor_name = name) constructors
  (* | AlgebraicType (_adt_name, constructors, Some dat_t) ->
      List.exists
        (fun (ctor_name, _arg_types) ->
          ctor_name ^ "_" ^ type_descriptor_of_perktype dat_t = name)
        constructors *)
  | _ ->
      Utils.say_here
        (Printf.sprintf
           "Type %s is not an algebraic type, so it cannot have constructors"
           (show_perktype typ));
      false

let is_equatable_type (typ : perktype) : bool =
  match typ |> resolve_type |> discard_type_aq with
  | AlgebraicType _ -> false
  | _t ->
      (* Printf.printf "Type %s is equatable" (show_perktype ([], t, [])); *)
      true
