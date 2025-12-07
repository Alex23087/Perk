(** Utils for the var symbol table *)

open Ast
open Errors

type symtable_t = (perkident, perktype) Hashtbl.t

(** List of hash tables, one for each scope. The head of the table is the
    current scope.*)
let var_symbol_table : symtable_t list ref = ref []

(** Adds a symbol to the [symbol_table] symbol table. Used for scoping.*)
let push_symbol_table_local symbol_table =
  symbol_table := Hashtbl.create 10 :: !symbol_table

(** Adds a symbol to the var symbol table. Used for scoping.*)
let push_symbol_table () = push_symbol_table_local var_symbol_table

(** Removes the head of the [symbol_table] symbol table. Used for scoping.*)
let pop_symbol_table_local symbol_table = symbol_table := List.tl !symbol_table

(** Removes the head of the var symbol table. Used for scoping.*)
let pop_symbol_table () = pop_symbol_table_local var_symbol_table

(** Looks up a variable [id] in the symbol table [symbol_table].*)
let lookup_var_local symbol_table (id : perkident) : perktype option =
  let rec lookup_in_tables tables =
    match tables with
    | [] -> None
    | h :: t ->
        if Hashtbl.mem h id then Some (Hashtbl.find h id)
        else lookup_in_tables t
  in
  lookup_in_tables !symbol_table

(** Looks up a variable in the var symbol table *)
let lookup_var = lookup_var_local var_symbol_table

(** Debug function for printing a symbol table *)
let print_symbol_table_local symbol_table =
  Printf.printf "Symbol Table:\n";
  let print_table table =
    Hashtbl.iter
      (fun id typ ->
        Printf.printf "Identifier: %s, Type: %s\n" id (Codegen.codegen_type typ))
      table
  in
  List.iter print_table !symbol_table

(** Debug function for printing the var symbol table *)
let print_symbol_table () = print_symbol_table_local var_symbol_table

(** Given a symbol table [symbol_table], and identifier [id] and its type [t],
    it binds [id] in [symbol_table] with type [t] if it is not already defined.
*)
let bind_var_local symbol_table (id : perkident) (t : perktype) =
  match !symbol_table with
  | [] -> failwith "No symbol table available"
  | h :: _ ->
      all_vars := (id, t) :: !all_vars;
      if Hashtbl.mem h id then
        raise (Double_declaration ("Identifier already defined: " ^ id))
      else Hashtbl.add h id t
(* ;print_symbol_table () *)

(** Binds a variable in the var symbol table. *)
let bind_var = bind_var_local var_symbol_table

(** Given a symbol table [symbol_table], and identifier [id] and its type [t],
    it checks whether [id] is already defined in [symbol_table], and if so
    rebinds it with type [t].*)
let rebind_var_local symbol_table (id : perkident) (t : perktype) =
  match !symbol_table with
  | [] -> failwith "No symbol table available"
  | h :: _ ->
      if Hashtbl.mem h id then Hashtbl.replace h id t
      else raise (Undeclared ("Identifier wasn't defined: " ^ id))

(** Rebinds a variable in the var_symbol_table *)
let rebind_var = rebind_var_local var_symbol_table

(** Returns a list of all global identifiers*)
let get_all_global_identifiers () : string list =
  let rec get_all_global_identifiers_aux symbol_table =
    match symbol_table with
    | tbl :: [] ->
        Hashtbl.fold (fun id (_typ, _code, _deps) acc -> id :: acc) tbl []
    | _tbl :: t -> get_all_global_identifiers_aux t
    | [] -> failwith "No symbol table available"
  in
  get_all_global_identifiers_aux !var_symbol_table

(** Adds all symbols from a table to another *)
let append_symbol_table (dst_table : symtable_t list ref)
    (src_table : symtable_t) =
  Hashtbl.iter (fun id typ -> bind_var_local dst_table id typ) src_table
