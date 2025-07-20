(** Utils for the type symbol table *)

open Ast
open Errors
open Utils

let builtin_types =
  [
    ("void", (void_type, None));
    ("int", (int_type, None));
    ("float", (float_type, None));
    ("char", (char_type, None));
  ]

let builtin_types_unlabeled = [ void_type; int_type; float_type; char_type ]

let rem_ptr s =
  let len = String.length s in
  if len < 4 then failwith "should not happen" else String.sub s 0 (len - 4)
(* maybe should check the string is "_ptr" *)

let rec is_builtin_type k (typ, code) =
  match typ with
  | _, Pointertype typ1, _ ->
      let k = rem_ptr k in
      is_builtin_type k (typ1, code)
  | _ -> List.mem (k, (typ, code)) builtin_types

let rec is_builtin_type_unlabeled typ =
  match typ with
  | _, Pointertype typ1, _ -> is_builtin_type_unlabeled typ1
  | _ -> List.mem typ builtin_types_unlabeled

let type_symbol_table : (perkident, perktype * string option) Hashtbl.t =
  let tbl = Hashtbl.create 10 in
  List.iter (fun (id, t) -> Hashtbl.add tbl id t) builtin_types;
  tbl

let lookup_type (id : perkident) : perktype option =
  if Hashtbl.mem type_symbol_table id then
    let t, _code = Hashtbl.find type_symbol_table id in
    Some t
  else None

let resolve_count, resolve_hit, resolve_miss = (ref 0, ref 0, ref 0)

let resolve_type (typ : perktype) : perktype =
  let type_resolve_table : (perktype, perktype) Hashtbl.t = Hashtbl.create 10 in
  let rec resolve_type_aux ?(unfold : int = 0) (typ : perktype)
      (lst : perktype list) : perktype * perktype list =
    resolve_count := !resolve_count + 1;
    say_here (Printf.sprintf "resolve_type: %s" (show_perktype typ));
    if unfold <= 0 then (typ, typ :: lst)
    else
      match Hashtbl.find_opt type_resolve_table typ with
      | Some t ->
          resolve_hit := !resolve_hit + 1;
          (t, t :: lst)
      | None ->
          resolve_miss := !resolve_miss + 1;
          let resolved_type, visited =
            if List.mem typ lst then (typ, lst)
            else
              let a, typ', q = typ in
              match typ' with
              | Basetype t ->
                  ( ( a,
                      (match lookup_type t with
                      | None -> typ'
                      | Some (_, t, _) -> t),
                      q ),
                    typ :: lst )
              | Pointertype t ->
                  let lst = typ :: lst in
                  let res_t, res_l =
                    resolve_type_aux ~unfold:(unfold - 1) t lst
                  in
                  ((a, Pointertype res_t, q), res_l)
              | Funtype (params, ret) ->
                  let lst = typ :: lst in
                  let params_t, params_l =
                    List.fold_right
                      (fun param (acc, lst) ->
                        let res_t, res_l =
                          resolve_type_aux ~unfold:(unfold - 1) param lst
                        in
                        (res_t :: acc, res_l))
                      params ([], lst)
                  in
                  let ret_t, ret_l =
                    resolve_type_aux ~unfold:(unfold - 1) ret params_l
                  in
                  ((a, Funtype (params_t, ret_t), q), ret_l)
              | Lambdatype (params, ret, _free_vars) ->
                  let lst = typ :: lst in
                  let params_t, params_l =
                    List.fold_right
                      (fun param (acc, lst) ->
                        let res_t, res_l =
                          resolve_type_aux ~unfold:(unfold - 1) param lst
                        in
                        (res_t :: acc, res_l))
                      params ([], lst)
                  in
                  let ret_t, ret_l =
                    resolve_type_aux ~unfold:(unfold - 1) ret params_l
                  in
                  (* TODO: Check if free vars need to be resolved (empirically it would seem not) *)
                  ((a, Lambdatype (params_t, ret_t, _free_vars), q), ret_l)
              | Arraytype (t, n) ->
                  let lst = typ :: lst in
                  let ret_t, ret_l =
                    resolve_type_aux ~unfold:(unfold - 1) t lst
                  in
                  ((a, Arraytype (ret_t, n), q), ret_l)
              | Structtype _ -> (typ, lst)
              | ArcheType (name, decls) ->
                  let lst = typ :: lst in
                  let decls_t, decls_l =
                    List.fold_right
                      (fun (param, ide) (acc, lst) ->
                        let res_t, res_l =
                          resolve_type_aux ~unfold:(unfold - 1) param lst
                        in
                        ((res_t, ide) :: acc, res_l))
                      decls ([], lst)
                  in
                  ((a, ArcheType (name, decls_t), q), decls_l)
              | ArchetypeSum ts ->
                  let lst = typ :: lst in
                  let ts_t, ts_l =
                    List.fold_right
                      (fun param (acc, lst) ->
                        let res_t, res_l =
                          resolve_type_aux ~unfold:(unfold - 1) param lst
                        in
                        (res_t :: acc, res_l))
                      ts ([], lst)
                  in
                  ((a, ArchetypeSum ts_t, q), ts_l)
              | Modeltype (name, archetypes, decls, constr_params, member_funcs)
                ->
                  let lst = typ :: lst in
                  let decls_t, decls_l =
                    List.fold_right
                      (fun (attr, (param, ide)) (acc, lst) ->
                        let res_t, res_l =
                          resolve_type_aux ~unfold:(unfold - 1) param lst
                        in
                        ((attr, (res_t, ide)) :: acc, res_l))
                      decls ([], lst)
                  in
                  ( ( a,
                      Modeltype
                        (name, archetypes, decls_t, constr_params, member_funcs),
                      q ),
                    decls_l )
              | Optiontype t ->
                  let lst = typ :: lst in
                  let ret_t, ret_l =
                    resolve_type_aux ~unfold:(unfold - 1) t lst
                  in
                  ((a, Optiontype ret_t, q), ret_l)
              | Tupletype ts ->
                  let lst = typ :: lst in
                  let ts_t, ts_l =
                    List.fold_right
                      (fun param (acc, lst) ->
                        let res_t, res_l =
                          resolve_type_aux ~unfold:(unfold - 1) param lst
                        in
                        (res_t :: acc, res_l))
                      ts ([], lst)
                  in
                  ((a, Tupletype ts_t, q), ts_l)
              | Vararg -> ((a, Vararg, q), lst)
              | Infer -> ((a, Infer, q), lst)
          in
          Hashtbl.add type_resolve_table typ resolved_type;
          (resolved_type, visited)
    (* | Structtype t -> (
          match lookup_type t with None -> typ' | Some (_, t, _) -> t) *)
  in
  fst (resolve_type_aux ~unfold:2 typ [])

let rec c_type_of_base_type (t : perktype) : string =
  match t with
  | t when t = int_type -> "int"
  | t when t = void_type -> "void"
  | t when t = float_type -> "float"
  | t when t = char_type -> "char"
  | _, Pointertype t1, _ -> c_type_of_base_type t1 ^ "*"
  | _ -> failwith "not a base type"

(* Returns a C type for the input perktype. The types returned are the ones generated by the various synthesised typedefs *)
let rec type_descriptor_of_perktype ?(erase_env = true) (t : perktype) : string
    =
  let _, t, _ = t in
  match t with
  | Basetype s -> s
  | Structtype (id, _) -> id
  | Funtype (args, ret) ->
      let args_str =
        String.concat "__" (List.map type_descriptor_of_perktype args)
      in
      Printf.sprintf "l_%s_to_%s_r" args_str (type_descriptor_of_perktype ret)
  | Lambdatype (_args, _ret, free_vars) ->
      let lambda_type_desc =
        type_descriptor_of_perktype (functype_of_lambdatype ([], t, []))
      in
      let environment_type_desc =
        type_descriptor_of_environment ~erase_env free_vars
      in
      let capture_type_desc = lambda_type_desc ^ "_" ^ environment_type_desc in
      capture_type_desc
  | Pointertype t -> Printf.sprintf "%s_ptr" (type_descriptor_of_perktype t)
  | Arraytype (t, None) ->
      Printf.sprintf "%s_arr" (type_descriptor_of_perktype t)
  | Arraytype (t, Some n) ->
      Printf.sprintf "%s_%d_arr" (type_descriptor_of_perktype t) n
  | Vararg ->
      "vararg"
      (* This is probably problematic, cannot define function pointers with ... . Nvm, apparently you can 😕*)
  | ArcheType (name, _decls) -> name
  | ArchetypeSum archs ->
      "Sum" ^ String.concat "Plus" (List.map type_descriptor_of_perktype archs)
  | Modeltype (name, _archs, _decls, _constr_params, _member_funcs) -> name
  | Optiontype t -> Printf.sprintf "%s_opt" (type_descriptor_of_perktype t)
  | Infer ->
      raise
        (Not_inferred
           "Impossible: type has not been inferred in \
            type_descriptor_of_perktype")
  | Tupletype ts ->
      Printf.sprintf "tup_%s_le"
        (String.concat "__" (List.map type_descriptor_of_perktype ts))

and c_type_of_perktype ?(erase_env = true) (t : perktype) =
  if is_builtin_type_unlabeled t then c_type_of_base_type t
  else type_descriptor_of_perktype ~erase_env t

and type_descriptor_of_environment ?(erase_env = false)
    (free_vars : perkvardesc list) : string =
  if erase_env then "env"
  else
    "env_"
    ^ String.concat "_"
        (List.map (fun (typ, _id) -> type_descriptor_of_perktype typ) free_vars)

(* Prints the symbol table 🤯 *)
let print_type_symbol_table () =
  Printf.printf "Type Symbol Table:\n";
  Hashtbl.iter
    (fun id (typ, _code) ->
      Printf.printf "%s: %s,\n\n" id (type_descriptor_of_perktype typ))
    type_symbol_table

(* Binds a type in the symble table. NOT Throws an exception if the name has already been defined *)
let bind_type (t : perktype) =
  say_here (Printf.sprintf "bind_type: %s" (show_perktype t));
  let id = type_descriptor_of_perktype t in
  (* if not (Hashtbl.mem type_symbol_table id) then *)
  Hashtbl.add type_symbol_table id (t, None)
(* else
    match t with
    | _, Basetype _, _
    | _, Pointertype _, _
    | _, Funtype _, _
    | _, Arraytype _, _
    | _, Structtype _, _
    | _, ArchetypeSum _, _
    | _, Optiontype _, _
    | _, Tupletype _, _
    | _, Vararg, _
    | _, Infer, _ ->
        ()
    | _, ArcheType _, _ | _, Modeltype _, _ ->
        raise
          (Double_declaration (Printf.sprintf "Type %s already declared" id)) *)

(* Replaces a type in the symbol table. Throws an exception if the name is not bound. Used by typecheck_deferred_function to replace temporary function types *)
let rebind_type (id : perkident) (t : perktype) =
  if Hashtbl.mem type_symbol_table id then
    Hashtbl.replace type_symbol_table id (t, None)
  else raise (Undeclared ("Type not found in symbol table: " ^ id))

let used_counter, unused_counter, called_counter = (ref 0, ref 0, ref 0)

(* Returns the dependencies of a type. This is used to sort them when generating the typedefs for the program.*)
let dependencies_of_type (typ : perktype) : perkident list =
  called_counter := !called_counter + 1;
  let type_dep_table : (perktype, perkident list) Hashtbl.t =
    Hashtbl.create 10
  in

  (* Auxiliary functions that takes a list as input to avoid circular dependencies *)
  (* The voidize parameters controls whether models have to be erased to void*. This is needed to avoid circular dependencies *)
  let rec dependencies_of_type_aux ?(voidize : bool = false) (typ : perktype)
      (lst : perktype list) : perkident list * perktype list =
    let typ = resolve_type typ in
    say_here (Printf.sprintf "dependencies_of_type: %s\n\n" (show_perktype typ));
    match Hashtbl.find_opt type_dep_table typ with
    | Some ls ->
        used_counter := !used_counter + 1;
        (ls, typ :: lst)
    | None ->
        unused_counter := !unused_counter + 1;
        (* print_endline "unUsed deps hashtable"; *)
        let deps, visited =
          if List.mem typ lst then
            ( (match typ with
              | _, Basetype _, _ -> []
              | _ -> [ type_descriptor_of_perktype typ ]),
              lst )
          else
            let _, typ', _ = typ in
            match typ' with
            | Basetype _ -> ([], typ :: lst)
            | Pointertype t -> dependencies_of_type_aux ~voidize t (typ :: lst)
            | Funtype (params, ret) ->
                let lst = typ :: lst in
                let params_t, params_l =
                  List.fold_right
                    (fun param (acc, lst) ->
                      let res_t, res_l =
                        dependencies_of_type_aux ~voidize:true param lst
                      in
                      (res_t @ acc, res_l))
                    params ([], lst)
                in
                let ret_t, ret_l =
                  dependencies_of_type_aux ~voidize:true ret (ret :: params_l)
                in
                ((type_descriptor_of_perktype typ :: params_t) @ ret_t, ret_l)
            | Lambdatype (params, ret, _free_vars) ->
                let lst = typ :: lst in
                let params_t, params_l =
                  List.fold_right
                    (fun param (acc, lst) ->
                      let res_t, res_l =
                        dependencies_of_type_aux ~voidize:true param lst
                      in
                      (res_t @ acc, res_l))
                    params ([], lst)
                in
                let ret_t, ret_l =
                  dependencies_of_type_aux ~voidize:true ret (ret :: params_l)
                in
                let underlying_deps, underlying_l =
                  dependencies_of_type_aux ~voidize:true
                    (functype_of_lambdatype typ)
                    lst
                in
                ( (type_descriptor_of_perktype typ :: params_t)
                  @ ret_t @ underlying_deps,
                  ret_l @ underlying_l )
            | Arraytype (t, _) ->
                dependencies_of_type_aux ~voidize t (typ :: lst)
            | Structtype _ -> ([], lst) (* TODO: Struct dependencies *)
            | ArcheType (name, decls) ->
                let lst = typ :: lst in
                let decls_t, decls_l =
                  List.fold_right
                    (fun (param, _ide) (acc, lst) ->
                      let res_t, res_l =
                        dependencies_of_type_aux ~voidize param lst
                      in
                      (res_t @ acc, res_l))
                    decls ([], lst)
                in
                (name :: decls_t, decls_l)
            | ArchetypeSum ts ->
                let lst = typ :: lst in
                List.fold_left
                  (fun (acc, lst) param ->
                    let res_t, res_l =
                      dependencies_of_type_aux ~voidize param lst
                    in
                    (res_t @ acc, res_l))
                  ([ type_descriptor_of_perktype typ ], lst)
                  ts
            | Modeltype (name, archetypes, decls, constr_params, _) ->
                let lst = typ :: lst in
                if voidize then ([], lst)
                else
                  let decls =
                    List.map
                      (* TODO: Check very carefully (TOODOO: BE MORE SPECIFIC WITH THE TOODOOS) *)
                      (fun (attr, (typ, id)) ->
                        match typ with
                        | _a, Lambdatype (_params, _ret, _), _d ->
                            ( attr,
                              (add_parameter_to_func (self_type name) typ, id)
                            )
                        | _ -> (attr, (typ, id)))
                      decls
                  in
                  let decls_t, lst =
                    List.fold_right
                      (fun (_attr, (param, _ide)) (acc, lst) ->
                        let res_t, res_l =
                          dependencies_of_type_aux ~voidize param lst
                        in
                        (res_t @ acc, res_l))
                      decls ([], lst)
                  in
                  let constructor_params_t, constr_params_l =
                    List.fold_left
                      (fun (acc, lst) param ->
                        let res_t, res_l =
                          dependencies_of_type_aux ~voidize param lst
                        in
                        (res_t @ acc, res_l))
                      ([], lst) constr_params
                  in
                  ( (name :: archetypes) @ decls_t @ constructor_params_t,
                    constr_params_l )
            | Optiontype t ->
                let deps, visited =
                  dependencies_of_type_aux ~voidize:true t (typ :: lst)
                in
                (type_descriptor_of_perktype typ :: deps, visited)
            | Tupletype ts ->
                let lst = typ :: lst in
                List.fold_left
                  (fun (acc, lst) param ->
                    let res_t, res_l =
                      dependencies_of_type_aux ~voidize param lst
                    in
                    (res_t @ acc, res_l))
                  ([ type_descriptor_of_perktype typ ], lst)
                  ts
            | Vararg -> ([], lst)
            | Infer -> ([], lst)
        in
        Hashtbl.add type_dep_table typ deps;
        (* Printf.printf "Type Dependency Table:\n";
           Hashtbl.iter
             (fun typ deps ->
               Printf.printf "%s: [%s]\n" (show_perktype typ)
                 (String.concat ", " deps))
             type_dep_table; *)
        (deps, visited)
  in
  fst (dependencies_of_type_aux typ [])
  (* Remove duplicates *)
  |> List.sort_uniq String.compare
  (* Remove the type itself from the dependencies *)
  |> List.filter (fun s -> s <> type_descriptor_of_perktype typ)

(* Binds a type in the type symbol table, only if it is a non-native type (e.g., tuples, options, models, etc.) *)
let rec bind_type_if_needed (typ : perktype) =
  match typ with
  | [], Infer, [] -> ()
  | _ -> (
      match lookup_type (type_descriptor_of_perktype ~erase_env:false typ) with
      | Some _ -> ()
      | None -> (
          say_here
            (Printf.sprintf "bind_type_if_needed: %s" (show_perktype typ));
          let typ' = resolve_type typ in
          match typ' with
          | _, Basetype _t, _ -> ()
          | _, Pointertype t, _ ->
              bind_type typ;
              bind_type_if_needed t
          | _, Funtype (_params, _ret), _ ->
              bind_type typ';
              List.iter bind_type_if_needed _params;
              bind_type_if_needed _ret
          | _, Lambdatype (_params, _ret, _free_variables), _ ->
              bind_type typ';
              (* Bind the type of the underlying function *)
              bind_type_if_needed (functype_of_lambdatype typ);
              (* Bind the parameters and return type of the lambda *)
              List.iter bind_type_if_needed _params;
              bind_type_if_needed _ret
          | _, Arraytype (t, _), _ ->
              bind_type typ';
              bind_type_if_needed t
          | _, Structtype (_id, fields), _ ->
              bind_type typ;
              List.iter (fun (typ, _id) -> bind_type_if_needed typ) fields
          | _, ArcheType (_name, _decls), _ ->
              bind_type typ';
              List.iter (fun (typ, _id) -> bind_type_if_needed typ) _decls
          | _, ArchetypeSum _ts, _ ->
              bind_type typ';
              List.iter bind_type_if_needed _ts
          | ( _,
              Modeltype
                (_name, _archetypes, _decls, _constr_params, _member_funcs),
              _ ) ->
              bind_type typ'
              (* ; List.iter (fun (typ, _id) -> bind_type_if_needed typ) decls;
                 List.iter (fun typ -> bind_type_if_needed typ) constr_params *)
          | _, Optiontype t, _ ->
              bind_type typ';
              bind_type_if_needed t
          | _, Tupletype ts, _ ->
              bind_type typ';
              List.iter bind_type_if_needed ts
          | _, Vararg, _ -> ()
          | _, Infer, _ -> ()))

(* Manually add code to the binding. Used by codegen functions for Models and Archetypes, where the struct code is generated during regular codegen, instead of at the end like for simple synthesized types like tuples and functions *)
let add_code_to_type_binding (_typ : perktype) (code : string) : unit =
  bind_type_if_needed _typ;
  let key = type_descriptor_of_perktype _typ in
  let _t, _code = Hashtbl.find type_symbol_table key in
  Hashtbl.replace type_symbol_table key (_t, Some code)
