open Ast

(** bounds for generic types -- add more *)
type bound = Numeric_Bound [@@deriving show]

type generic_type = {
  bounds : bound list;
  inferred_type : perktype option;
}
[@@deriving show]
(** type of generic types *)

(** Hash table of currently generic types, their bounds and inferred exact type*)
let generic_types_table : (perktype, generic_type) Hashtbl.t = Hashtbl.create 10

type file_info = {
  lambdas_hashmap : (expr_a, string * string * string list * string) Hashtbl.t;
      (** table of lambdas: Lambda expression, identifier, generated code,
          capture list, type_descriptor*)
  public_fundecl_symbol_table : (perkident, perktype) Hashtbl.t;
  private_fundecl_symbol_table : (perkident, perktype) Hashtbl.t;
  import_list : string list ref;
  polyfun_instances : (string, (perktype * bool) list) Hashtbl.t;
      (** id -> (type, was_codegen'd) list *)
  file_local_polyfuns : (string, perktype list * perktype * perktype) Hashtbl.t;
  polyfuns_to_be_defined : (topleveldef_a * perktype) list ref;
  polyfun_bounds : (string, generic_type) Hashtbl.t;

  polyadt_instances : (string, (perktype * bool) list) Hashtbl.t;
  polyadt_declared : (string, perktype * (perkident * perktype list) list) Hashtbl.t
}

let allocate_file_info () =
  ref
    {
      lambdas_hashmap = Hashtbl.create 10;
      public_fundecl_symbol_table = Hashtbl.create 10;
      private_fundecl_symbol_table = Hashtbl.create 10;
      import_list = ref [];
      polyfun_instances = Hashtbl.create 10;
      file_local_polyfuns = Hashtbl.create 10;
      polyfuns_to_be_defined = ref [];
      polyfun_bounds = Hashtbl.create 10;
      polyadt_instances = Hashtbl.create 10;
      polyadt_declared = Hashtbl.create 10
    }

let current_file_info = allocate_file_info ()
let save_file_info () = !current_file_info
let restore_file_info f = current_file_info := f
let set_new_file_info () = current_file_info := !(allocate_file_info ())

(* Getters *)
let get_lambdas_hashmap () = !current_file_info.lambdas_hashmap

let get_public_fundecl_symbol_table () =
  !current_file_info.public_fundecl_symbol_table

let get_private_fundecl_symbol_table () =
  !current_file_info.private_fundecl_symbol_table

let get_import_list () = !current_file_info.import_list
let get_polyfun_instances () = !current_file_info.polyfun_instances
let get_file_local_polyfuns () = !current_file_info.file_local_polyfuns
let get_polyfuns_to_be_defined () = !current_file_info.polyfuns_to_be_defined
let get_polyfun_bounds () = !current_file_info.polyfun_bounds
let get_polyadt_instances () = !current_file_info.polyadt_instances
let get_polyadt_declared () = !current_file_info.polyadt_declared

let set_polyfun_as_codegened id t =
  let remove_first x lst =
    let rec aux acc = function
      | [] -> List.rev acc
      | hd :: tl when hd = x -> List.rev_append acc tl
      | hd :: tl -> aux (hd :: acc) tl
    in
    aux [] lst
  in
  let pi = get_polyfun_instances () in
  let instances = try Hashtbl.find pi id with Not_found -> [] in
  Hashtbl.replace pi id ((t, true) :: (remove_first (t, false)) instances)

let polyfun_is_already_codegened id t =
  let pi = get_polyfun_instances () in
  let instances = try Hashtbl.find pi id with Not_found -> [] in
  List.mem (t, true) instances
