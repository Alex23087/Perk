(** Typechecks a set of toplevel definitions, instancing the inferred types. *)

open Ast
open Errors
open Utils
open Type_symbol_table
open Var_symbol_table
open Free_variables
open Parse_tags
open Parse_lexing_commons

(** List of library functions and their types :
    [(perkident * perktype) list ref]*)
let library_functions = ref []

(** List of import paths *)
let import_path_list = ref []

(** gathers the path(s) where libraries are located *)
let get_lib_path s =
  let remove_first_last s =
    let len = String.length s in
    if len <= 2 then "" else String.sub s 1 (len - 2)
  in
  (* TODO actual includepaths *)
  Printf.sprintf "/usr/include/%s" (remove_first_last s)

(** Hash table of the defined ADT constructors*)
let defined_constructors : (perkident, unit) Hashtbl.t = Hashtbl.create 10

(* TODO handle type aliases *)

(** check if type is integral *)
let is_integral (_, typ, _) =
  let nums =
    [
      Basetype "int";
      Basetype "size_t";
      Basetype "int64_t";
      Basetype "int32_t";
      Basetype "int16_t";
      Basetype "int8_t";
      Basetype "uint64_t";
      Basetype "uint32_t";
      Basetype "uint16_t";
      Basetype "uint8_t";
      Basetype "char";
    ]
  in
  List.mem typ nums

(** check if type is numerical *)
let is_numerical (_, typ, _) =
  let nums =
    [
      Basetype "int";
      Basetype "float";
      Basetype "double";
      Basetype "size_t";
      Basetype "int64_t";
      Basetype "int32_t";
      Basetype "int16_t";
      Basetype "int8_t";
      Basetype "uint64_t";
      Basetype "uint32_t";
      Basetype "uint16_t";
      Basetype "uint8_t";
      Basetype "char";
    ]
  in
  List.mem typ nums

(* TODO: This ranking cannot be done with a total order, signed and unsigned are not comparable *)

(** provides an integer ranking of numerical types: higher values are most
    general types *)
let numerical_rank : perktype -> int = function
  | _, Basetype "double", _ -> 12
  | _, Basetype "float", _ -> 11
  | _, Basetype "int64_t", _ -> 10
  | _, Basetype "size_t", _ -> 9
  | _, Basetype "uint64_t", _ -> 8
  | _, Basetype "int32_t", _ -> 7
  | _, Basetype "uint32_t", _ -> 6
  | _, Basetype "int", _ -> 5 (* “int” you can treat as 32‐bit *)
  | _, Basetype "int16_t", _ -> 4
  | _, Basetype "uint16_t", _ -> 3
  | _, Basetype "int8_t", _ -> 2
  | _, Basetype "char", _ ->
      2 (* char's signedness is implementation dependent *)
  | _, Basetype "uint8_t", _ -> 1
  | _ -> 0 (* non‐numeric or unknown *)

(** typechecks a set of toplevel definitions, instancing the inferred types *)
let rec typecheck_program (ast : topleveldef_a list) : topleveldef_a list =
  push_symbol_table ();
  let res = List.map typecheck_topleveldef ast in
  let res = List.map typecheck_deferred_function res in
  (* Will it do it in the right order?? *)
  (* print_symbol_table (); *)
  (* print_type_symbol_table (); *)
  res

(** Typechecks functions after everything else *)
and typecheck_deferred_function (tldf : topleveldef_a) : topleveldef_a =
  match ( $ ) tldf with
  | Fundef (ret_type, id, params, body) ->
      push_symbol_table ();
      List.iter
        (fun (typ, id) ->
          try bind_var id typ
          with Double_declaration msg -> raise_type_error tldf msg)
        params;
      let body_res, _body_type, body_returns =
        typecheck_command ~retype:(Some ret_type) body
      in
      if (not body_returns) && not (is_unit_type ret_type) then
        raise_type_error tldf "Not all code paths return a value";
      pop_symbol_table ();
      let funtype =
        ([], Funtype (List.map (fun (typ, _) -> typ) params, ret_type), [])
      in
      rebind_var id funtype;
      rebind_type (type_descriptor_of_perktype funtype) funtype;
      annot_copy tldf (Fundef (ret_type, id, params, body_res))
  | _ -> tldf

(** Typechecks toplevel definitions *)
and typecheck_topleveldef (tldf : topleveldef_a) : topleveldef_a =
  match ( $ ) tldf with
  | Import s ->
      import_path_list := get_lib_path s :: !import_path_list;
      generate_tags !import_path_list;
      library_functions := get_prototype_types ();
      (* for each library function, if it is not already defined define it *)
      (* TODO solve conditionally compiled definitions *)
      (* TODO hoist these*)
      List.iter
        (fun (id, t) ->
          if Option.is_none (lookup_var id) then bind_var id t else ())
        !library_functions;
      remove_tags ();
      remove_libs_expanded ();
      tldf
  | Open _ ->
      raise_compilation_error tldf
        "Opens should not reach this point (typecheck). If you see this error, \
         please open an issue at https://github.com/Alex23087/Perk/issues"
  | InlineC _ -> tldf
  | Def (((typ, id), expr), _) ->
      if id = "self" then raise_type_error tldf "Identifier self is reserved"
      else
        let typ' = resolve_type typ in
        let expr_res, expr_type = typecheck_expr expr in
        let expr_type =
          match (typ', expr_type) with
          | _, (_, Infer, _) -> typ'
          | (_, Infer, _), _ -> expr_type
          | _ -> expr_type
        in
        let array_init =
          match (typ', ( $ ) expr_res) with
          | (_, Arraytype (_, Some _n), _), Array _ -> true
          | _ -> false
        in
        let typ'' =
          try match_types ~coalesce:true ~array_init typ' expr_type
          with Type_match_error msg -> raise_type_error tldf msg
        in
        let typ''_nocoal =
          try match_types ~coalesce:false ~array_init typ' expr_type
          with Type_match_error _ -> ([], Infer, [])
        in
        let deftype =
          if equal_perktype typ'' typ''_nocoal then None else Some typ''
        in
        (match discard_type_aq typ'' with
        | Lambdatype (params, _, _) ->
            if
              List.exists
                (fun typ ->
                  match discard_type_aq typ with
                  | Basetype "void" -> true
                  | _ -> false)
                params
            then raise_type_error tldf "Cannot have void parameters in a lambda"
        | _ -> ());
        bind_type_if_needed typ';
        bind_type_if_needed typ'';
        bind_var id typ'';
        annot_copy tldf (Def (((typ'', id), expr_res), deftype))
  | Fundef (ret_type, id, params, body) ->
      if id = "self" then raise_type_error tldf "Identifier self is reserved"
      else (
        if
          List.exists
            (fun (typ, _) ->
              match discard_type_aq typ with
              | Basetype "void" -> true
              | _ -> false)
            params
        then raise_type_error tldf "Cannot have void parameters in a function";
        let funtype =
          ([], Funtype (List.map (fun (typ, _) -> typ) params, ret_type), [])
        in
        bind_var id funtype;
        bind_type_if_needed funtype;
        annot_copy tldf (Fundef (ret_type, id, params, body))
        (* |> ignore; typecheck_deferred_function tldf *))
  | Extern (id, typ) ->
      (if id = "self" then raise_type_error tldf "Identifier self is reserved"
       else
         match lookup_var id with
         | Some _ ->
             raise_type_error tldf
               (Printf.sprintf "Identifier %s is already defined" id)
         | None -> ());
      bind_var id typ;
      bind_type_if_needed typ;
      tldf
      (* annotate_dummy Skip *)
      (* Externs are only useful for type checking. No need to keep it for codegen step *)
  | Archetype (name, decls) -> (
      if name = "self" then raise_type_error tldf "Identifier self is reserved"
      else
        match lookup_type name with
        | Some _ ->
            raise_type_error tldf
              (Printf.sprintf "Archetype %s is already defined" name)
        | None ->
            List.iter bind_type_if_needed
              (List.map
                 (fun d ->
                   d |> decl_of_declorfun
                   |> (fun (typ, id) ->
                   (match discard_type_aq typ with
                   | Funtype (params, _ret) ->
                       if
                         List.exists
                           (fun typ ->
                             match discard_type_aq typ with
                             | Basetype "void" -> true
                             | _ -> false)
                           params
                       then
                         raise_type_error tldf
                           "Cannot have void parameters in a function"
                   | Lambdatype (params, _, _) ->
                       if
                         List.exists
                           (fun typ ->
                             match discard_type_aq typ with
                             | Basetype "void" -> true
                             | _ -> false)
                           params
                       then
                         raise_type_error tldf
                           "Cannot have void parameters in a lambda"
                   | _ -> ());

                   (add_parameter_to_func_only void_pointer typ, id))
                   |> fst)
                 decls);
            bind_type_if_needed
              ([], ArcheType (name, List.map decl_of_declorfun decls), []);
            tldf)
  | Model (ident, archetypes, fields) ->
      (if ident = "self" then
         raise_type_error tldf "Identifier self is reserved"
       else
         (* Check that the model is not already defined *)
         let m = lookup_type ident in
         match m with
         | Some _ ->
             raise_type_error tldf
               (Printf.sprintf "Model %s is already defined" ident)
         | None -> ());

      (* Get implemented archetypes *)
      let archetypes_t =
        List.map
          (fun a ->
            match lookup_type a with
            | Some t -> (
                match t with
                | _, ArcheType _, _ -> t
                | _ ->
                    raise_type_error tldf
                      (Printf.sprintf
                         "Model %s is trying to implement non-archetype %s"
                         ident a))
            | None ->
                raise_type_error tldf
                  (Printf.sprintf
                     "Model %s implements non-existent Archetype %s" ident a))
          archetypes
      in

      (* Get all the fields that the model is required to implement *)
      let required_fields =
        List.flatten
          (List.map
             (fun (_, t, _) ->
               match t with
               | ArcheType (ident, params) ->
                   List.map (fun p -> (p, ident)) params
               | _ ->
                   failwith
                     "Impossible, archetypes were checked just a few lines \
                      before")
             archetypes_t)
      in

      (* Check that all the required fields are defined *)
      List.iter
        (fun ((typ, id), arch) ->
          match
            List.exists
              (fun def ->
                let _, (t, i) = decl_of_deforfun def in
                if id = i then
                  let _ =
                    try match_types t typ
                    with Type_match_error msg -> raise_type_error def msg
                  in
                  true
                else false)
              fields
          with
          | true -> ()
          | false ->
              raise_type_error tldf
                (Printf.sprintf
                   "Model %s is missing required field '%s' of type %s, \
                    declared in archetype %s"
                   ident id (show_perktype typ) arch))
        required_fields;

      (* Gather member functions *)
      let member_funcs = get_member_functions fields in

      (* Get constructor, if exists *)
      push_symbol_table ();
      bind_var "self"
        ( [],
          Modeltype
            ( ident,
              archetypes,
              List.map (fun d -> d |> decl_of_deforfun) fields,
              [],
              member_funcs ),
          [] );
      let constr =
        List.find_opt
          (fun def ->
            match ( $ ) def with
            | DefFun (_, (_, id, _, _)) -> id = "constructor"
            | DefVar (_, ((_, id), expr)) ->
                if id = "constructor" then
                  raise_type_error expr "Constructor must be a function"
                else false)
          fields
      in
      let constr_params =
        match Option.map ( $ ) constr with
        | Some (DefFun (_, (ret, _, params, _))) ->
            (* Check that constructor returns void *)
            let _ =
              try match_types ([], Basetype "void", []) ret
              with Type_match_error msg -> raise_type_error tldf msg
            in
            List.map fst params
        | Some _ ->
            raise_type_error (Option.get constr)
              "Constructor must be a function 2"
        | None -> []
      in
      pop_symbol_table ();

      (* Check that all the fields defined in the model are well-typed *)
      push_symbol_table ();
      (* !!!!!WARNING!!!!! THIS CANNOT BE DONE LIKE THAT. MUST BE CHECKED AS FOR PROGRAM FOR HOISTED FUNCTIONS !!!!!WARNING!!!!! *)
      let temp_model_type =
        ( [],
          Modeltype
            ( ident,
              archetypes,
              List.map decl_of_deforfun fields,
              constr_params,
              member_funcs ),
          [] )
      in
      bind_type_if_needed temp_model_type;
      bind_var "self" temp_model_type;
      (* The following local symbol table is used to check for duplicate definitions in the model, without accidentally defining names at a wrong level *)
      let local_symbol_table = ref [ Hashtbl.create 10 ] in
      let fields_res =
        List.map
          (fun def ->
            match ( $ ) def with
            | DefFun (attrs, (ret, id, params, body)) ->
                push_symbol_table ();
                if
                  List.exists
                    (fun (typ, _) ->
                      match discard_type_aq typ with
                      | Basetype "void" -> true
                      | _ -> false)
                    params
                then
                  raise_type_error tldf
                    "Cannot have void parameters in a function";
                List.iter
                  (fun (typ, id) ->
                    try bind_var id typ
                    with Double_declaration msg -> raise_type_error def msg)
                  params;
                let body_res, _body_type, body_returns =
                  typecheck_command ~retype:(Some ret) body
                in
                if (not body_returns) && not (is_unit_type ret) then
                  raise_type_error tldf "Not all code paths return a value";
                pop_symbol_table ();
                (try
                   bind_var_local local_symbol_table id
                     ([], Funtype (List.map fst params, ret), [])
                 with Double_declaration msg -> raise_type_error def msg);
                annot_copy def (DefFun (attrs, (ret, id, params, body_res)))
            | DefVar (attrs, ((typ, id), expr)) ->
                let expr_res, expr_type = typecheck_expr expr in
                let expr_res, expr_type = fill_nothing expr_res expr_type typ in
                (* TODO: usual array initialization doesn't work for models, as
                the fields are defined first and then assigned, and C doesn't allow
                this for array expressions. Figure out a way to deal with this *)
                let array_init =
                  match (typ, ( $ ) expr_res) with
                  | (_, Arraytype (_, Some _n), _), Array _ -> true
                  | _ -> false
                in
                let typ' =
                  try match_types ~array_init typ expr_type
                  with Type_match_error msg -> raise_type_error expr msg
                in
                (match discard_type_aq typ' with
                | Lambdatype (params, _, _) ->
                    if
                      List.exists
                        (fun typ ->
                          match discard_type_aq typ with
                          | Basetype "void" -> true
                          | _ -> false)
                        params
                    then
                      raise_type_error tldf
                        "Cannot have void parameters in a lambda"
                | _ -> ());
                (try bind_var_local local_symbol_table id typ'
                 with Double_declaration msg -> raise_type_error def msg);
                annot_copy def (DefVar (attrs, ((typ', id), expr_res))))
          fields
      in
      pop_symbol_table ();
      (* Add model to the symbol table *)
      let modeltype =
        ( [],
          Modeltype
            ( ident,
              archetypes,
              List.map decl_of_deforfun fields_res,
              constr_params,
              member_funcs ),
          [] )
      in
      rebind_type (type_descriptor_of_perktype modeltype) modeltype;
      List.iter
        (fun def ->
          match ( $ ) def with
          | DefFun (_, (typ, _, params, _)) ->
              (* Generate type binding for functions. Need to add virtual self to the parameters *)
              ([], Funtype (List.map fst params, typ), [])
              |> add_parameter_to_func modeltype
              |> bind_type_if_needed
          | DefVar (_, ((typ, _), _)) -> bind_type_if_needed typ)
        fields_res;
      annot_copy tldf (Model (ident, archetypes, fields_res))
  | Struct (ident, fields) ->
      let local_symbol_table = ref [ Hashtbl.create 10 ] in
      let (fields_res : perkdef list) =
        List.map
          (fun ((typ, id), expr) ->
            (try bind_var_local local_symbol_table id typ
             with Double_declaration msg -> raise_type_error tldf msg);
            let typ' = resolve_type typ in
            let expr_res, expr_type = typecheck_expr expr in
            let field_type = match_types typ' expr_type in
            ((field_type, id), expr_res))
          fields
      in
      bind_type_if_needed ([], Structtype (ident, fields_res), []);
      annot_copy tldf (Struct (ident, fields_res))
  | ADT (ident, constructors) ->
      (* TODO: check existence of types *)
      List.iter
        (fun (ide, t) ->
          if Hashtbl.mem defined_constructors ide then
            raise_type_error tldf
              (Printf.sprintf "Constructor %s is already defined" ide);
          Hashtbl.add defined_constructors ide ();
          bind_var ide
            ([], Funtype (t, ([], AlgebraicType (ident, constructors), [])), []))
        constructors;
      bind_type_if_needed ([], AlgebraicType (ident, constructors), []);
      tldf

(** Typechecks commands
    @param retype the expected return type
    @return
      the typechecked command (can be modified by the check), the return type
      (if any), and whether all code paths return a value *)
and typecheck_command ?(retype : perktype option = None) (cmd : command_a) :
    command_a * perktype option * bool =
  match ( $ ) cmd with
  | InlineCCmd _ -> (cmd, None, true)
  | Block c ->
      push_symbol_table ();
      let c_res, return_type, does_return = typecheck_command ~retype c in
      pop_symbol_table ();
      (annot_copy cmd (Block c_res), return_type, does_return)
  | DefCmd (((typ, id), expr), _) ->
      if id = "self" then raise_type_error cmd "Identifier self is reserved"
      else
        let typ' = resolve_type typ in
        let expr_res, expr_type = typecheck_expr expr in
        let expr_res, expr_type = fill_nothing expr_res expr_type typ' in
        let expr_type =
          match (typ', expr_type) with
          | _, (_, Infer, _) -> typ'
          | (_, Infer, _), _ -> expr_type
          | _ -> expr_type
        in
        let array_init =
          match (typ', ( $ ) expr_res) with
          | (_, Arraytype (_, Some _n), _), Array _ -> true
          | _ -> false
        in
        let typ'' =
          try match_types ~coalesce:true ~array_init typ' expr_type
          with Type_match_error msg -> raise_type_error cmd msg
        in
        let typ''_nocoal =
          try match_types ~coalesce:false ~array_init typ' expr_type
          with Type_match_error _ -> ([], Infer, [])
        in
        let deftype =
          if equal_perktype typ'' typ''_nocoal then None else Some typ''
        in
        (match discard_type_aq typ'' with
        | Lambdatype (params, _, _) ->
            if
              List.exists
                (fun typ ->
                  match discard_type_aq typ with
                  | Basetype "void" -> true
                  | _ -> false)
                params
            then raise_type_error cmd "Cannot have void parameters in a lambda"
        | _ -> ());
        bind_type_if_needed typ';
        bind_type_if_needed typ'';
        bind_var id typ'';
        (* Printf.printf "DefCmd: %s, deftype: %s\n" id
           (match deftype with
           | Some deftype -> show_perktype deftype
           | None -> "None"); *)
        (annot_copy cmd (DefCmd (((typ'', id), expr_res), deftype)), None, false)
  | Assign (lhs, rhs, _, _) ->
      let lhs_res, lhs_type = typecheck_expr lhs in
      let rhs_res, rhs_type = typecheck_expr rhs in
      let rhs_res, rhs_type = fill_nothing rhs_res rhs_type lhs_type in
      let exprval =
        try match_types ~coalesce:true lhs_type rhs_type
        with Type_match_error msg -> raise_type_error cmd msg
      in
      let exprval_nocoal =
        try match_types ~coalesce:false lhs_type rhs_type
        with Type_match_error _ -> ([], Infer, [])
      in
      let acctype =
        match ( $ ) lhs_res with Access (_, _, t, _) -> t | _ -> None
      in
      let rasstype =
        if equal_perktype exprval exprval_nocoal then None else Some exprval
      in
      (* Printf.printf "Assign: %s = %s, acctype: %s, rasstype: %s\n"
         (show_perktype lhs_type) (show_perktype rhs_type)
         (match acctype with Some t -> show_perktype t | None -> "None")
         (match rasstype with Some t -> show_perktype t | None -> "None"); *)
      ( annot_copy cmd (Assign (lhs_res, rhs_res, acctype, rasstype)),
        None,
        false )
  | Seq (c1, c2) ->
      let c1_res, c1_type, c1_returns = typecheck_command ~retype c1 in
      (* TODO: If c1_returns is true, c2 is dead code. Warn *)
      let c2_res, c2_type, c2_returns = typecheck_command ~retype c2 in
      let return_type =
        match (c1_type, c2_type) with
        | Some t, Some t' -> (
            try Some (match_types t t')
            with Type_match_error msg -> raise_type_error cmd msg)
        | Some t, None | None, Some t -> Some t
        | None, None -> None
      in
      ( annot_copy cmd (Seq (c1_res, c2_res)),
        return_type,
        c1_returns || c2_returns )
  | IfThenElse (guard, then_branch, else_branch) ->
      let guard_res, guard_type = typecheck_expr guard in
      (match guard_type with
      | _, Basetype "bool", _ -> ()
      | g when is_numerical g -> ()
      | _ ->
          raise_type_error cmd
            (Printf.sprintf "If guard must be a boolean or an int, got %s"
               (show_perktype guard_type)));
      push_symbol_table ();
      let then_branch_res, _then_branch_type, then_branch_returns =
        typecheck_command ~retype then_branch
      in
      pop_symbol_table ();
      push_symbol_table ();
      let else_branch_res, _else_branch_type, else_branch_returns =
        typecheck_command ~retype else_branch
      in
      pop_symbol_table ();
      ( annot_copy cmd (IfThenElse (guard_res, then_branch_res, else_branch_res)),
        retype,
        then_branch_returns && else_branch_returns )
  | Whiledo (guard, body) ->
      let guard_res, guard_type = typecheck_expr guard in
      (match guard_type with
      | _, Basetype "bool", _ -> ()
      | g when is_numerical g -> ()
      | _ ->
          raise_type_error cmd
            (Printf.sprintf "While guard must be a boolean or an int, got %s"
               (show_perktype guard_type)));
      push_symbol_table ();
      let body_res, body_type, body_returns = typecheck_command ~retype body in
      pop_symbol_table ();
      (annot_copy cmd (Whiledo (guard_res, body_res)), body_type, body_returns)
  | Dowhile (guard, body) ->
      let guard_res, guard_type = typecheck_expr guard in
      (match guard_type with
      | _, Basetype "bool", _ -> ()
      | g when is_numerical g -> ()
      | _ ->
          raise_type_error cmd
            (Printf.sprintf "While guard must be a boolean or an int, got %s"
               (show_perktype guard_type)));
      push_symbol_table ();
      let body_res, body_type, body_returns = typecheck_command ~retype body in
      pop_symbol_table ();
      (annot_copy cmd (Dowhile (guard_res, body_res)), body_type, body_returns)
  | For (initcmd, guard, incrcmd, body) ->
      push_symbol_table ();
      let initcmd_res, _initcmd_type, initcmd_returns =
        typecheck_command ~retype initcmd
      in
      let guard_res, guard_type = typecheck_expr guard in
      (match guard_type with
      | _, Basetype "bool", _ -> ()
      | g when is_numerical g -> ()
      | _ ->
          raise_type_error cmd
            (Printf.sprintf "For guard must be a boolean or an int, got %s"
               (show_perktype guard_type)));
      let incrcmd_res, _incrcmd_type, incrcmd_returns =
        typecheck_command ~retype incrcmd
      in
      let body_res, body_type, body_returns = typecheck_command ~retype body in
      pop_symbol_table ();
      ( annot_copy cmd (For (initcmd_res, guard_res, incrcmd_res, body_res)),
        body_type,
        (* TODO: Check agreement between body_type and other types *)
        body_returns || initcmd_returns || incrcmd_returns )
  | Expr e -> (annot_copy cmd (Expr (fst (typecheck_expr e))), None, false)
  | Switch _ -> (cmd, None, false)
  | Skip -> (cmd, None, false)
  | Banish id ->
      (* TODO: Let banish unbind the future (unbind banished things after they're banished) *)
      (match Option.map resolve_type (lookup_var id) with
      | None -> raise_syntax_error cmd ("Identifier " ^ id ^ " not found")
      | Some (_, Modeltype _, _) -> ()
      | Some _ ->
          raise_syntax_error cmd
            (Printf.sprintf "Variable %s is not a model" id));
      (cmd, None, false)
  | Return None -> (
      match retype with
      | Some (_, Basetype "void", _) | None -> (cmd, None, true)
      | Some t ->
          raise_type_error cmd
            (Printf.sprintf
               "This return is supposed to return a value of type %s, but it's \
                empty"
               (show_perktype t)))
  | Return (Some e) ->
      let e_res, e_type = typecheck_expr e in
      (match retype with
      | Some (_, Basetype "void", _) | None ->
          raise_type_error cmd "This return is not supposed to return any value"
      | Some t ->
          ignore
            (try match_types t e_type
             with Type_match_error _msg ->
               raise_type_error cmd
                 (Printf.sprintf
                    "This return is supposed to return a value of type %s, got \
                     %s instead"
                    (show_perktype t) (show_perktype e_type))));
      (annot_copy cmd (Return (Some e_res)), Some e_type, true)
  | Continue | Break -> (cmd, None, false)
  | Match (e, match_entry_list, _) ->
      let expr, expr_type = typecheck_expr e in
      let entry_list, rettype, does_return =
        typecheck_match_entry_list ~retype expr_type match_entry_list
      in
      ( annot_copy cmd (Match (expr, entry_list, Some expr_type)),
        rettype,
        does_return )

and typecheck_match_entry_list ?(retype : perktype option = None)
    (match_type : perktype) (mel : match_entry_a list) =
  let check_all = List.map (typecheck_match_entry ~retype match_type) mel in
  let firsts = List.map (fun (a, _, _) -> a) check_all in
  let seconds = List.map (fun (_, b, _) -> b) check_all in
  let thirds = List.map (fun (_, _, c) -> c) check_all in

  let all_same =
    match seconds with
    | [] -> None
    | hd :: tl ->
        (let hd = Option.map resolve_type hd in
         List.iter (fun x ->
             match (Option.map resolve_type x, Option.map resolve_type hd) with
             | Some x, Some hd -> (
                 try match_types x hd |> ignore
                 with Type_match_error _ ->
                   raise_type_error (List.hd firsts)
                     (Printf.sprintf
                        "Entries of match have different return types: %s and \
                         %s"
                        (show_perktype x) (show_perktype hd)))
             | None, None -> ()
             | Some x, None | None, Some x ->
                 raise_type_error (List.hd firsts)
                   (Printf.sprintf
                      "Entries of match have different return types: %s and \
                       None"
                      (show_perktype x))))
          tl;
        hd
  in
  let all_return = List.for_all (fun x -> x) thirds in

  (firsts, all_same, all_return)

and typecheck_match_entry ?(retype : perktype option = None)
    (match_type : perktype) (entry : match_entry_a) =
  match ( $ ) entry with
  (* | Default c ->
      let body_res, body_type, body_returns = typecheck_command c in
      (annot_copy entry (Default body_res), body_type, body_returns) *)
  | MatchCase (case, _when_expr, c) ->
      let case = typecheck_match_case match_type case in
      push_symbol_table ();
      let vars_in_case = get_vars_in_case case in
      List.iter (fun (x, t) -> bind_var x t) vars_in_case;
      let body_res, body_type, body_returns = typecheck_command ~retype c in
      pop_symbol_table ();
      ( annot_copy entry (MatchCase (case, _when_expr, body_res)),
        body_type,
        body_returns )

and typecheck_match_case (match_type : perktype) case =
  match ( $ ) case with
  | Matchall -> case
  | MatchVar (x, Some typ) -> (
      try
        let _ = match_types typ match_type in
        case
      with Type_match_error _msg ->
        raise_type_error case
          (Printf.sprintf "Variable %s has type %s but it should have type %s" x
             (show_perktype typ) (show_perktype match_type)))
  | MatchVar (x, None) ->
      (* matchvar type inference *)
      annot_copy case (MatchVar (x, Some match_type))
  | MatchExpr e -> (
      let e1, t = typecheck_expr e in
      Printf.printf "MatchExpr type: %s\n" (show_perktype t);
      if not (is_equatable_type t) then
        raise_type_error case
          (Printf.sprintf "Expression in case has non-equatable type %s"
             (show_perktype t));
      try
        let _ = match_types t match_type in
        annot_copy case (MatchExpr e1)
      with Type_match_error _msg ->
        raise_type_error case
          (Printf.sprintf
             "Expression in case has type %s but it should have type %s"
             (show_perktype t) (show_perktype match_type)))
  | CompoundCase (id, cases) ->
      if is_constructor_name id then
        if is_constructor_of id match_type then (
          let parameters =
            match resolve_type match_type with
            | _, AlgebraicType (_, constructors), _ ->
                List.find (fun (ide, _) -> ide = id) constructors |> snd
            | _ -> failwith "Impossible, is_constructor_of returned true"
          in
          if List.length parameters <> List.length cases then
            raise_type_error case
              (Printf.sprintf
                 "Constructor %s expects %d parameters, got %d instead" id
                 (List.length parameters) (List.length cases));
          (* Typecheck each case *)
          annot_copy case
            (CompoundCase
               ( id,
                 List.mapi
                   (fun i c -> typecheck_match_case (List.nth parameters i) c)
                   cases )))
        else
          raise_type_error case
            (Printf.sprintf "Constructor %s does not construct type %s" id
               (show_perktype match_type))
      else
        let smart_msg =
          let lvid = lookup_var id in
          if Option.is_some lvid then
            let t = Option.get lvid in
            Printf.sprintf
              " To access the variable %s : %s use the syntax `{%s}" id
              (show_perktype t) id
          else ""
        in

        raise_compilation_error case
          (Printf.sprintf "%s id not a valid constructor.%s" id smart_msg)

and get_vars_in_case (case : match_case_a) =
  match ( $ ) case with
  | MatchVar (v, Some t) -> [ (v, t) ]
  | MatchVar (_, None) ->
      failwith "should not happen, match variable has not been inferred (3)"
  | CompoundCase (_, cl) -> List.map get_vars_in_case cl |> List.flatten
  | _ -> []

(** Typechecks expressions *)
and typecheck_expr ?(expected_return : perktype option = None) (expr : expr_a) :
    expr_a * perktype =
  match ( $ ) expr with
  | Bool _ -> (expr, ([], Basetype "bool", []))
  | Int _ -> (expr, ([], Basetype "int", []))
  | Float _ -> (expr, ([], Basetype "float", []))
  | Char _ -> (expr, ([], Basetype "char", []))
  | String _ -> (expr, ([], Pointertype ([], Basetype "char", []), []))
  | Var id -> (
      match lookup_var id with
      | Some (([], Funtype (params, ret), []) as t) -> (
          match Option.map discard_type_aq expected_return with
          | Some (Funtype _) -> (expr, t)
          | _ ->
              (* Constructors are automatically applied *)
              if Hashtbl.mem defined_constructors id && List.length params = 0
              then (annot_copy expr (Apply (expr, [], None)), ret)
              else (expr, t))
      | Some t -> (expr, t)
      | None -> raise_type_error expr ("Unknown identifier: " ^ id))
  | Apply (func, params, _) ->
      let fun_expr, fun_type =
        typecheck_expr
          ~expected_return:(Some ([], Funtype ([], void_type), []))
            (* TODO: Put correct type here *)
          func
      in
      (* Check that the function is a function 👍 *)
      let fun_param_types, fun_ret_type, apptype =
        match fun_type with
        | _, Funtype (param_types, ret_type), _ -> (param_types, ret_type, None)
        | _, Lambdatype (param_types, ret_type, _), _ ->
            (param_types, ret_type, Some fun_type)
        | _ -> raise_type_error func "Function type expected"
      in
      let param_rets =
        try typecheck_expr_list params fun_param_types
        with Invalid_argument _ ->
          raise_type_error expr
            (Printf.sprintf
               "Wrong number of parameters passed to function: expected %d, \
                got %d"
               (List.length fun_param_types)
               (List.length params))
      in
      let _param_types =
        try match_type_list fun_param_types param_rets
        with Type_match_error msg -> raise_type_error expr msg
      in
      let param_rets =
        List.map2
          (fun (e, t1) t2 -> fill_nothing e t1 t2)
          param_rets _param_types
      in
      ( annot_copy expr (Apply (fun_expr, List.map fst param_rets, apptype)),
        fun_ret_type )
  | Binop (op, lhs, rhs) -> (
      let cast_priority t1 t2 =
        let r1 = numerical_rank t1 in
        let r2 = numerical_rank t2 in
        if r1 >= r2 then t1 (* cast t2 → t1 *) else t2
        (* cast t1 → t2 *)
      in

      match op with
      | Add | Sub | Mul | Div ->
          let lhs_res, lhs_type = typecheck_expr lhs in
          let rhs_res, rhs_type = typecheck_expr rhs in
          let lhs_res, lhs_type, rhs_res, rhs_type =
            if is_numerical lhs_type && is_numerical rhs_type then
              let winner_type = cast_priority lhs_type rhs_type in
              ( annot_copy lhs_res (Cast ((lhs_type, winner_type), lhs_res)),
                winner_type,
                annot_copy rhs_res (Cast ((rhs_type, winner_type), rhs_res)),
                winner_type )
            else raise_type_error rhs "Numerical type expected"
          in
          let res_type =
            try match_types lhs_type rhs_type
            with Type_match_error msg -> raise_type_error expr msg
          in
          let lhs_res, _lhs_type = fill_nothing lhs_res lhs_type res_type in
          let rhs_res, _rhs_type = fill_nothing rhs_res rhs_type res_type in
          (annot_copy expr (Binop (op, lhs_res, rhs_res)), res_type)
      | Eq | Lt | Leq | Gt | Geq | Neq | Land | Lor ->
          (* these comparisons all return bool *)
          let lhs_res, _ = typecheck_expr lhs in
          let rhs_res, _ = typecheck_expr rhs in
          ( annot_copy expr (Binop (op, lhs_res, rhs_res)),
            ([], Basetype "bool", []) ))
  | PreUnop (op, e) ->
      let expr_res, expr_type = typecheck_expr e in
      let res_type =
        match (op, resolve_type expr_type) with
        | Dereference, (_, Pointertype t, _) -> t
        | Reference, t -> ([], Pointertype t, [])
        | Dereference, _ ->
            raise_type_error expr
              (Printf.sprintf "Cannot dereference non-pointer type %s"
                 (show_perktype expr_type))
        | _, t -> t
      in
      (annot_copy expr (PreUnop (op, expr_res)), res_type)
  | Lambda (retype, params, body, _) ->
      push_symbol_table ();
      List.iter
        (fun (typ, id) ->
          try bind_var id typ
          with Double_declaration msg -> raise_type_error expr msg)
        params;
      let body_res, _body_type, body_returns =
        typecheck_command ~retype:(Some retype) body
      in
      if (not body_returns) && not (is_unit_type retype) then
        raise_type_error expr "Not all code paths return a value";
      let free_vars = fst (free_variables_expr expr) in
      let free_vars =
        List.map
          (fun v ->
            match lookup_var v with
            | Some vt -> (vt, v)
            | None ->
                raise_type_error expr (Printf.sprintf "Unbound variable %s" v))
          free_vars
      in
      (* if a lambda has no free variables, it is made into a function *)
      let lamtype =
        match free_vars with
        | [] ->
            if static_compilation then
              ([], Funtype (List.map (fun (typ, _) -> typ) params, retype), [])
            else
              ( [],
                Lambdatype
                  (List.map (fun (typ, _) -> typ) params, retype, free_vars),
                [] )
        | _ ->
            ( [],
              Lambdatype
                (List.map (fun (typ, _) -> typ) params, retype, free_vars),
              [] )
      in
      pop_symbol_table ();
      bind_type_if_needed lamtype;
      autocast
        (annot_copy expr (Lambda (retype, params, body_res, free_vars)))
        lamtype expected_return
  | PostUnop (op, e) ->
      let expr_res, expr_type = typecheck_expr e in
      let op, res_type =
        match (op, resolve_type expr_type) with
        | OptionGet _, (_, Optiontype t, _) -> (OptionGet (Some t), t)
        | OptionIsSome, (_, Optiontype _t, _) -> (op, ([], Basetype "bool", []))
        | OptionGet _, _ | OptionIsSome, _ ->
            raise_type_error expr
              (Printf.sprintf "Option operator requires option type, got %s"
                 (show_perktype expr_type))
        | _, t -> (op, t)
      in
      (annot_copy expr (PostUnop (op, expr_res)), res_type)
  | Parenthesised e ->
      let e1, typ = typecheck_expr ~expected_return e in
      (annot_copy expr (Parenthesised e1), typ)
  | Subscript (container, accessor) -> (
      let container_res, container_type = typecheck_expr container in
      let accessor_res, accessor_type = typecheck_expr accessor in
      if is_integral accessor_type then ()
      else
        raise_type_error expr
          (Printf.sprintf "Subscript operator requires integer type, got %s"
             (show_perktype accessor_type));
      match container_type with
      | _, Arraytype (t, _n), _ ->
          (annot_copy expr (Subscript (container_res, accessor_res)), t)
      | _, Pointertype t, _ ->
          (annot_copy expr (Subscript (container_res, accessor_res)), t)
      | _, Tupletype ts, _ -> (
          match ( $ ) accessor_res with
          | Int i ->
              if i < 0 || i >= List.length ts then
                raise_type_error expr
                  (Printf.sprintf "Subscript out of bounds: %d" i);
              ( annot_copy expr (TupleSubscript (container_res, i)),
                List.nth ts i )
          | _ ->
              raise_type_error expr
                "Subscript operator requires constant integer")
      | _ ->
          raise_type_error expr
            (Printf.sprintf
               "Subscript operator requires array, tuple or pointer, got %s"
               (show_perktype container_type)))
  | Summon (typeid, params) -> (
      let typ = lookup_type typeid in
      match typ with
      | Some
          ( attrs,
            Modeltype (_name, archetypes, fields, constr_params, member_funcs),
            specs ) ->
          let param_rets = List.map typecheck_expr params in
          say_here
            (Printf.sprintf "Summon: %s\n" (show_perktype (Option.get typ))
            ^ Printf.sprintf "constr_params: %s\n"
                (String.concat ", " (List.map show_perktype constr_params))
            ^ Printf.sprintf "params: %s\n"
                (String.concat ", "
                   (List.map show_perktype (List.map snd param_rets))));
          flush stdout;
          let param_rets =
            if List.length param_rets <> List.length constr_params then
              raise_type_error expr
                (Printf.sprintf
                   "Wrong number of parameters passed to constructor: expected \
                    %d, got %d%s"
                   (List.length constr_params)
                   (List.length param_rets)
                   (if
                      List.exists
                        (fun (_a, (_typ, id)) -> id = "constructor")
                        fields
                    then ""
                    else ". Constructor is not defined"))
            else
              List.map2
                (fun (a, b) c -> fill_nothing a b c)
                param_rets constr_params
          in
          let _ =
            try match_type_list constr_params param_rets
            with Type_match_error msg -> raise_type_error expr msg
          in
          ( annot_copy expr (Summon (typeid, List.map fst param_rets)),
            ( attrs,
              Modeltype (typeid, archetypes, fields, constr_params, member_funcs),
              specs ) )
      | Some _ ->
          raise_type_error expr
            (Printf.sprintf "Can only summon model types. %s is not a model."
               typeid)
      | None -> raise_type_error expr (Printf.sprintf "Unknown type: %s" typeid)
      )
  | Access (expr, ide, _, _) ->
      let expr_res, expr_type = typecheck_expr expr in
      let ( (res_type : perktype),
            (access_type : perktype option),
            (rightype : perktype option) ) =
        let is_self =
          match ( $ ) expr_res with Var "self" -> true | _ -> false
        in
        let expr_type' = resolve_type expr_type in
        match expr_type' with
        | ( _,
            Modeltype (name, _archetypes, fields, _constr_params, _member_funcs),
            _ ) -> (
            let field = List.find_opt (fun (_, (_, id)) -> id = ide) fields in
            match field with
            | Some (attrs, (typ, _))
              when is_self || not (List.mem Private attrs) ->
                (typ, Some expr_type, Some typ)
            | Some (_, (_, _)) ->
                raise_type_error expr
                  (Printf.sprintf
                     "Trying to access private field %s of model %s" ide name)
            | None ->
                raise_type_error expr
                  (Printf.sprintf "Field %s not found in model %s" ide name))
        | _, ArchetypeSum archetypes, _ -> (
            let archs_with_idents =
              List.map
                (fun a ->
                  match resolve_type a with
                  | _, ArcheType (_aid, decls), [] ->
                      List.map (fun (t, id) -> (a, t, id)) decls
                  | _ ->
                      failwith
                        "Impossible: Model is implementing a non-archetype")
                archetypes
              |> List.flatten
            in
            match
              List.find_opt (fun (_arch, _t, id) -> id = ide) archs_with_idents
            with
            | Some (arch, t, _id) -> (t, Some arch, Some t)
            | None ->
                raise_type_error expr
                  (Printf.sprintf
                     "Field %s not found in archetypes implemented by variable"
                     ide))
        (* | _, ArcheType (_name, decls), _ -> (
            match List.find_opt (fun (_t, id) -> id = ide) decls with
            | Some (t, _id) -> (t, Some (resolve_type expr_type))
            | None ->
                raise_type_error expr
                  (Printf.sprintf
                     "Field %s not found in archetypes implemented by variable"
                     ide)) *)
        | _, Arraytype (_t, Some _n), _ when ide = "length" ->
            (([], Basetype "int", []), Some expr_type, None)
        | _, Arraytype (_t, None), _ when ide = "length" ->
            raise_type_error expr
              "Cannot access length of an array with unknown size"
        | _, Structtype (name, fields), _ -> (
            let field = List.find_opt (fun ((_, id), _) -> id = ide) fields in
            match field with
            | Some ((typ, _), _) -> (typ, Some expr_type, Some typ)
            | None ->
                raise_type_error expr
                  (Printf.sprintf "Field %s not found in struct %s" ide name))
        | _ ->
            raise_type_error expr
              (Printf.sprintf "Cannot access field %s of non-model type %s" ide
                 (show_perktype expr_type))
      in
      (annot_copy expr (Access (expr_res, ide, access_type, rightype)), res_type)
  | Tuple (exprs, _) ->
      let exprs_res = List.map typecheck_expr exprs in
      let exprs_res = List.map (fun (a, b) -> fill_nothing a b b) exprs_res in
      (* TODO: This won't work on typles (and wtf is THAT supposed to mean?!?) *)
      let types = List.map snd exprs_res in
      let tupletype = ([], Tupletype types, []) in
      bind_type_if_needed tupletype;
      ( annot_copy expr (Tuple (List.map fst exprs_res, Some tupletype)),
        ([], Tupletype types, []) )
  | TupleSubscript _ ->
      failwith
        "Should not happen: this variant is only generated by the typechecker"
  | As (expr, archs, _) -> (
      bind_type_if_needed ([], ArchetypeSum archs, []);
      let expr, typ = typecheck_expr expr in
      (* This function is used by both the model and archetype sum cases.
      It checks the presence of all archetypes needed, and tags the As expression with
      the type of the variable, since the expression has to be compiled according to its type *)
      let check_archetypes (archetypes : perkident list) =
        let archs_idents =
          List.map
            (fun a ->
              match resolve_type a with
              | _, ArcheType (name, _decls), _ -> name
              | _ ->
                  failwith
                    (Printf.sprintf "Impossible: archetype expected. Got %s"
                       (show_perktype a)))
            archs
        in
        List.iter
          (fun arch ->
            match List.find_opt (fun id -> id = arch) archetypes with
            | Some _ -> ()
            | None ->
                raise_type_error expr
                  (Printf.sprintf "Archetype %s not found in entity of type %s"
                     arch (show_perktype typ)))
          archs_idents;
        ( annot_copy expr (As (expr, archs, Some typ)),
          ([], ArchetypeSum archs, []) )
      in
      match typ with
      | _, Modeltype (_name, archetypes, _fields, _constr_params, _), _ ->
          check_archetypes archetypes
      | _, ArchetypeSum archetypes, _ ->
          let archetype_names =
            List.map
              (fun a ->
                match resolve_type a with
                | _, ArcheType (name, _decls), _ -> name
                | _ ->
                    failwith
                      (Printf.sprintf "Impossible: archetype expected. Got %s"
                         (show_perktype a)))
              archetypes
          in
          check_archetypes archetype_names
      | _ ->
          raise_type_error expr
            (Printf.sprintf
               "Cannot ~> on this entity, as it is neither a model type nor an \
                archetype sum"))
  | Something (e, _) ->
      let e_res, e_type = typecheck_expr e in
      (annot_copy expr (Something (e_res, e_type)), ([], Optiontype e_type, []))
  | Nothing typ ->
      (expr, typ) (* this used to be infer by default, but added "of" syntax *)
  | Array exprs -> (
      match exprs with
      | [] -> (expr, ([], Infer, []))
      | x :: xs ->
          let xexpr, xtyp = typecheck_expr x in
          let constant_list = List.init (List.length xs) (fun _ -> xtyp) in
          let exprs_res = List.map typecheck_expr xs in
          let exprs_e = List.map fst exprs_res in
          (* let exprs_t = List.map snd exprs_res in *)
          let _ =
            try match_type_list constant_list exprs_res
            with Type_match_error msg -> raise_type_error expr msg
          in
          let arraytype =
            ([], Arraytype (xtyp, Some (List.length xs + 1)), [])
          in
          bind_type_if_needed arraytype;
          (annot_copy expr (Array (xexpr :: exprs_e)), arraytype))
  | Cast (t, e) ->
      bind_type_if_needed (snd t);
      (annot_copy expr (Cast (t, fst (typecheck_expr e))), snd t)
  | IfThenElseExpr (guard, then_e, else_e) ->
      let guard_res, guard_type =
        typecheck_expr ~expected_return:(Some bool_type) guard
      in
      (match guard_type with
      | _, Basetype "bool", _ -> ()
      | g when is_numerical g -> ()
      | _ ->
          raise_type_error expr
            (Printf.sprintf "If guard must be a boolean or an int, got %s"
               (show_perktype guard_type)));
      let then_e_res, then_e_type = typecheck_expr then_e in
      let else_e_res, else_e_type = typecheck_expr else_e in
      let res_type =
        try match_types then_e_type else_e_type
        with Type_match_error msg -> raise_type_error else_e msg
      in
      let then_e_res, _then_e_type =
        fill_nothing then_e_res then_e_type res_type
      in
      let else_e_res, _else_e_type =
        fill_nothing else_e_res else_e_type res_type
      in
      ( annot_copy expr (IfThenElseExpr (guard_res, then_e_res, else_e_res)),
        res_type )
  | Make (id, inits) -> (
      let structype = lookup_type id in
      let structype =
        match structype with
        | Some t -> t
        | None ->
            raise_type_error expr (Printf.sprintf "Struct %s is not defined" id)
      in

      match structype with
      | _, Structtype (_name, fields), _ ->
          let local_symbol_table = ref [ Hashtbl.create 10 ] in
          List.iter
            (fun (id, expr) ->
              (try bind_var_local local_symbol_table id ([], Infer, [])
               with Double_declaration _msg ->
                 raise_type_error expr
                   (Printf.sprintf "Trying to initialize field %s twice" id));
              let field =
                List.assoc_opt id
                  (List.map (fun ((typ, id), _) -> (id, typ)) fields)
              in
              match field with
              | Some typ -> (
                  let _expr_res, expr_type = typecheck_expr expr in
                  try match_types typ expr_type |> ignore
                  with Type_match_error msg -> raise_type_error expr msg)
              | _ ->
                  raise_type_error expr
                    (Printf.sprintf "Field %s not found in struct %s" id _name))
            inits;
          List.map (fun ((typ, _), _) -> typ) fields |> ignore;
          (expr, structype)
      | _ ->
          raise_type_error expr
            (Printf.sprintf "Cannot make struct %s, as it is not defined" id))

(** Typechecks parameters *)
and typecheck_expr_list (exprs : expr_a list) (types : perktype list) :
    (expr_a * perktype) list =
  let typecheck_expr_list_aux (exps : expr_a list) (typs : perktype list) =
    match (exps, typs) with
    | [], [] -> []
    | [], [ (_, Vararg, _) ] -> []
    | [], _ | _, [] ->
        raise
          (Invalid_argument
             (Printf.sprintf "Wrong number of parameters: expected %d got %d"
                (List.length exprs) (List.length types)))
    | x :: xs, [ (_, Vararg, _) ] ->
        let x_res, x_type = typecheck_expr x in
        (x_res, x_type) :: typecheck_expr_list xs typs
    | x :: xs, t :: ts ->
        let x_res, x_type = typecheck_expr ~expected_return:(Some t) x in
        (x_res, x_type) :: typecheck_expr_list xs ts
  in
  typecheck_expr_list_aux exprs types

and fill_nothing (expr : expr_a) (exprtyp : perktype) (typ : perktype) :
    expr_a * perktype =
  let expr = autoas expr exprtyp typ in
  match (( $ ) expr, typ) with
  | Nothing _, ([], Optiontype _, []) -> (annot_copy expr (Nothing typ), typ)
  | Nothing _, _ ->
      raise_type_error expr "Nothing can only be used with Optiontype"
  | _ -> (expr, exprtyp)

(* Add more type checking logic as needed: pepperepeppe     peppè! culo*)

(** Checks if two types are the same or not. *)
and match_types ?(coalesce : bool = false) ?(array_init : bool = false)
    (expected : perktype) (actual : perktype) : perktype =
  let expected = resolve_type expected in
  let actual = resolve_type actual in
  let rec match_types_aux expected actual =
    (* This catches the case where one type has not been bound yet and thus results as a basetype *)
    let equal =
      try
        type_descriptor_of_perktype expected
        = type_descriptor_of_perktype actual
      with Not_inferred _ -> false
    in
    if equal then actual
    else
      let (_, expected', _), (_, actual', _) = (expected, actual) in
      match (expected', actual') with
      | Basetype t1, Basetype t2 when t1 = t2 -> actual
      | Pointertype t1, Pointertype t2 when t1 = t2 -> actual
      | Funtype (params1, ret1), Funtype (params2, ret2)
        when List.length params1 = List.length params2 ->
          let param_types = List.map2 match_types_aux params1 params2 in
          let ret_type = match_types_aux ret1 ret2 in
          ([], Funtype (param_types, ret_type), [])
      | Arraytype (t1, n1), Arraytype (t2, n2) when n1 = n2 || Option.is_none n1
        ->
          let t = match_types_aux t1 t2 in
          ([], Arraytype (t, n2), [])
      | Arraytype (t1, Some n1), Arraytype (t2, Some n2)
        when t1 = t2 && array_init && n1 >= n2 ->
          ([], Arraytype (t1, Some n1), [])
      | Structtype (t1, _), Structtype (t2, _) when t1 = t2 ->
          actual (* TODO: Needs deeper matching *)
      | ArcheType (name1, decls1), ArcheType (name2, decls2)
        when name1 = name2 && List.length decls1 = List.length decls2 ->
          let decls_types =
            List.map2
              (fun (t1, id1) (t2, id2) ->
                if id1 = id2 then (match_types_aux t1 t2, id1)
                else
                  raise
                    (Type_match_error
                       (Printf.sprintf
                          "Archetype %s has different field names: %s and %s"
                          name1 id1 id2)))
              decls1 decls2
          in
          ([], ArcheType (name1, decls_types), [])
      | ( Modeltype (name1, archetypes1, decls1, constr_params1, member_funcs1),
          Modeltype (name2, archetypes2, decls2, constr_params2, member_funcs2)
        )
        when name1 = name2
             && List.length archetypes1 = List.length archetypes2
             && List.length decls1 = List.length decls2
             && List.length constr_params1 = List.length constr_params2
             && List.equal String.equal member_funcs1 member_funcs2 ->
          let decls_types =
            List.map2
              (fun (a1, (t1, id1)) (a2, (t2, id2)) ->
                if a1 = a2 then
                  if id1 = id2 then (a1, (match_types_aux t1 t2, id1))
                  else
                    raise
                      (Type_match_error
                         (Printf.sprintf
                            "Model %s has different field names: %s and %s"
                            name1 id1 id2))
                else
                  raise
                    (Type_match_error
                       (Printf.sprintf "Model %s has different attributes" name1)))
              decls1 decls2
          in
          let constr_types =
            List.map2 match_types_aux constr_params1 constr_params2
          in
          ( [],
            Modeltype
              (name1, archetypes1, decls_types, constr_types, member_funcs1),
            [] )
      | Vararg, Vararg -> actual
      | Infer, _ | _, Infer -> actual
      | Optiontype t, Optiontype s -> ([], Optiontype (match_types_aux t s), [])
      | Tupletype t1, Tupletype t2 ->
          ([], Tupletype (List.map2 match_types_aux t1 t2), [])
      | ArchetypeSum t1, ArchetypeSum t2 -> (
          try
            let _ =
              match_type_list (t1 @ [ vararg ])
                (* A shorter archetype list can be matched with a longer one.
                   To implement this reusing existing code, we use the code for varargs *)
                (List.map (fun t -> (annotate_dummy (Int (-1)), t)) t2)
            in
            ([], ArchetypeSum t1, [])
          with Type_match_error _ | Type_error _ ->
            raise
              (Type_match_error
                 (Printf.sprintf "Type mismatch: expected %s,\ngot %s instead"
                    (Codegen.codegen_type ~expand:true expected)
                    (Codegen.codegen_type ~expand:true actual))))
      | ArchetypeSum t1, Modeltype (_, archetypes, _, _, _) -> (
          let t2 =
            List.map (fun id -> resolve_type ([], Basetype id, [])) archetypes
          in
          try
            let _ =
              match_type_list (t1 @ [ vararg ])
                (* A shorter archetype list can be matched with a longer one.
                   To implement this reusing existing code, we use the code for varargs *)
                (List.map (fun t -> (annotate_dummy (Int (-1)), t)) t2)
            in
            ([], ArchetypeSum t1, [])
          with Type_match_error _ | Type_error _ ->
            raise
              (Type_match_error
                 (Printf.sprintf "Type mismatch: expected %s,\ngot %s instead"
                    (Codegen.codegen_type ~expand:true expected)
                    (Codegen.codegen_type ~expand:true actual))))
      | Lambdatype (params1, ret1, _), Funtype (params2, ret2) ->
          let param_types = List.map2 match_types_aux params1 params2 in
          let ret_type = match_types_aux ret1 ret2 in
          ([], Lambdatype (param_types, ret_type, []), [])
      | Lambdatype (params1, ret1, free1), Lambdatype (params2, ret2, _) ->
          let param_types = List.map2 match_types_aux params1 params2 in
          let ret_type = match_types_aux ret1 ret2 in
          ([], Lambdatype (param_types, ret_type, free1), [])
      | Pointertype (_, Basetype "void", _), Modeltype _ -> actual
      | _ ->
          raise
            (Type_match_error
               (Printf.sprintf "Type mismatch: expected %s,\ngot %s instead"
                  (Codegen.codegen_type ~expand:true expected)
                  (Codegen.codegen_type ~expand:true actual)))
    (* (show_perktype expected)
                  (show_perktype actual))) *)
  in
  match (coalesce, expected, actual) with
  | _, (_, Optiontype t, _), (_, Optiontype s, _) ->
      ([], Optiontype (match_types_aux t s), [])
  | true, (_, Optiontype t, _), _ ->
      ([], Optiontype (match_types_aux t actual), [])
  | _, _, _ -> match_types_aux expected actual

(** CHecks if an argument list and a type list have the same types elementwise.
*)
and match_type_list (expected : perktype list)
    (actual : (expr_a * perktype) list) : perktype list =
  let rec match_type_list_aux expected' actual' =
    say_here
      (Printf.sprintf "match_type_list_aux: expected: %s, actual: %s"
         (String.concat ", " (List.map show_perktype expected'))
         (String.concat ", " (List.map (fun (_, t) -> show_perktype t) actual')));
    match (expected', actual') with
    | ([], Vararg, []) :: _ :: _, _ ->
        raise_type_error
          (fst (List.hd actual'))
          "Function has a vararg with other parameters after it"
    | [], [] -> []
    | [ ([], Vararg, []) ], [] -> []
    | [], _ ->
        raise_type_error
          (fst (List.hd actual'))
          (Printf.sprintf "Expected %d parameters, but got %d"
             (List.length expected) (List.length actual))
    | _, [] ->
        raise
          (Type_match_error
             (Printf.sprintf "Expected %d parameters, but got %d"
                (List.length expected) (List.length actual)))
    | [ ([], Vararg, []) ], a :: at ->
        snd a :: match_type_list_aux [ ([], Vararg, []) ] at
    | e :: et, a :: at ->
        let typ =
          try match_types e (snd a)
          with Type_match_error msg -> raise_type_error (fst a) msg
        in
        typ :: match_type_list_aux et at
  in
  match_type_list_aux expected actual

and autocast (expr : expr_a) (typ : perktype) (expected : perktype option) :
    expr_a * perktype =
  match expected with
  | None -> (expr, typ)
  | Some t -> (annot_copy expr (Cast ((typ, t), expr)), t)

and autoas (expr : expr_a) (actual : perktype) (expected : perktype) : expr_a =
  match (discard_type_aq expected, discard_type_aq actual) with
  | ArchetypeSum a1, ArchetypeSum _ | ArchetypeSum a1, Modeltype _ ->
      annot_copy expr (As (expr, a1, Some actual))
  | _ -> expr
