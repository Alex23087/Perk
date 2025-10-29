open Ast

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

(* Setters *)
let set_lambdas_hashmap v =
  current_file_info := { !current_file_info with lambdas_hashmap = v }

let set_fundecl_symbol_table v =
  current_file_info :=
    { !current_file_info with public_fundecl_symbol_table = v }

let set_private_fundecl_symbol_table fi v =
  fi := { !current_file_info with private_fundecl_symbol_table = v }

let set_import_list v =
  current_file_info := { !current_file_info with import_list = v }

let set_polyfun_instances v =
  current_file_info := { !current_file_info with polyfun_instances = v }

let set_file_local_polyfuns v =
  current_file_info := { !current_file_info with file_local_polyfuns = v }

let set_polyfuns_to_be_defined v =
  current_file_info := { !current_file_info with polyfuns_to_be_defined = v }

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
