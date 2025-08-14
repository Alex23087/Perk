(** Generates C code given a parsed Perk program. *)

open Ast
open Errors
open Utils
open Type_symbol_table

exception TypeError of string

let fst_4 (x, _, _, _) = x
let snd_4 (_, x, _, _) = x
let fst_3 (x, _, _) = x
let snd_3 (_, x, _) = x
let thrd_3 (_, _, x) = x
let swizzle (x, y) = (y, x)
let hashtbl_forall f h = Hashtbl.fold (fun k v acc -> f k v && acc) h true
let hashtbl_exists f h = Hashtbl.fold (fun k v acc -> f k v || acc) h false
let fresh_var_counter = ref 0

(** creates a new fresh variable __perk_s_n *)
let fresh_var (s : string) : string =
  let v = !fresh_var_counter in
  fresh_var_counter := v + 1;
  Printf.sprintf "__perk_%s_%d" s v

(** table of lambdas: Lambda expression, identifier, generated code, capture
    list, type_descriptor*)
let lambdas_hashmap : (expr_a, string * string * string list * string) Hashtbl.t
    =
  Hashtbl.create 10

(** Symbol table of function declarations *)
let fundecl_symbol_table : (perkident, perktype) Hashtbl.t = Hashtbl.create 10

(** list of imported libraries *)
let import_list : string list ref = ref []

(** hashtable of archetypes*)
let archetype_hashtable : (string, (string, perktype) Hashtbl.t) Hashtbl.t =
  Hashtbl.create 10

(** Adds a new archetype with name [name] to the archetype hastable *)
let add_archetype (name : string) : (string, perktype) Hashtbl.t =
  let new_archetype = Hashtbl.create 10 in
  Hashtbl.add archetype_hashtable name new_archetype;
  new_archetype

(* TODO: Remove this. This info can be retrieved from type_symbol_table *)
let get_archetype (name : string) : (string, perktype) Hashtbl.t =
  try Hashtbl.find archetype_hashtable name
  with Not_found ->
    raise (TypeError (Printf.sprintf "Archetype %s not found" name))

(** adds the binding [(id, typ)] to archetype with name [name]*)
let add_binding_to_archetype (name : string) (id : perkident) (typ : perktype) :
    unit =
  let archetype = get_archetype name in
  if not (Hashtbl.mem archetype id) then Hashtbl.add archetype id typ

(** Adds an import to the import list *)
let add_import (lib : string) : unit =
  if not (List.mem lib !import_list) then import_list := lib :: !import_list

let lambda_environments : (string * string) list ref = ref []
let lambda_typedefs : string list ref = ref []
let lambda_capture_dummies : (string * string) list ref = ref []

(** used to pass the let...in variables in codegen_expr *)
let generated_freevars = ref ""

(** codegens a function or a lambda *)
let rec codegen_functional ~(is_lambda : bool) (e : expr_a) : string =
  try fst_4 (Hashtbl.find lambdas_hashmap e)
  with Not_found -> (
    let id = fresh_var "lambda" in
    let compiled, (free_variables : perkvardesc list), type_descriptor =
      match ( $ ) e with
      | Lambda (retype, args, body, free_variables) -> (
          let type_str = codegen_type retype in
          let args_str =
            String.concat ", "
              (List.map
                 (fun (t, id) -> Printf.sprintf "%s %s" (codegen_type t) id)
                 args)
          in
          match free_variables with
          | [] when static_compilation ->
              let body_str = codegen_command body 1 in
              let lambdatype =
                ( [ Static ],
                  Funtype (List.map (fun (t, _) -> t) args, retype),
                  [] )
              in
              bind_function_type id lambdatype;
              let type_descriptor = type_descriptor_of_perktype lambdatype in
              ( Printf.sprintf "static %s %s(%s) {\n%s\n}" type_str id args_str
                  body_str,
                free_variables,
                type_descriptor )
          | _ ->
              (* Generate the string to "unpack" the environment (synthesized declarations of the form var = env.var) *)
              let env_bind_str =
                if List.length free_variables = 0 then "\n"
                else
                  "\n"
                  ^ (let envtype =
                       type_descriptor_of_environment ~erase_env:false
                         free_variables
                     in
                     Printf.sprintf "    %s* _env = (%s*) __lambdummy->env;\n"
                       envtype envtype)
                  ^ String.concat ";\n"
                      (List.mapi
                         (fun i (t, id) ->
                           Printf.sprintf "    %s %s = (%s)_env->_%d"
                             (codegen_type t) id (codegen_type t) i)
                         free_variables)
                  ^ ";\n"
              in
              let body_str = codegen_command body 1 in
              let lambdatype =
                ( [ Static ],
                  Lambdatype
                    (List.map (fun (t, _) -> t) args, retype, free_variables),
                  [] )
              in
              bind_function_type id lambdatype;
              let type_descriptor = type_descriptor_of_perktype lambdatype in
              (* codegen_lambda_capture e lambdatype; *)
              ( Printf.sprintf "static %s %s(%s) {%s%s\n}" type_str id args_str
                  env_bind_str body_str,
                free_variables,
                type_descriptor )
          (* with Not_inferred s -> raise_type_error e s *))
      | _ -> failwith "get_lambda: not a lambda expression"
    in
    Hashtbl.add lambdas_hashmap e
      (id, compiled, List.map snd free_variables, type_descriptor);
    match free_variables with
    | [] when static_compilation || not is_lambda -> id
    | _ ->
        let env_descriptor = type_descriptor_of_environment free_variables in
        let capture_list_str =
          String.concat ", " (List.map (fun (_, id) -> id) free_variables)
        in
        Printf.sprintf "((%s) alloclabmd(sizeof(%s), %s, (void*)&((%s) {%s})))"
          type_descriptor env_descriptor id env_descriptor capture_list_str)
(* Printf.sprintf "{{%s}, (%s) %s}"
   (String.concat ", "
      (List.map (fun s -> "(void*)" ^ s) free_variables))
   type_descriptor id) *)

(** Binds a function type to the [fundecl_symbol_table]. *)
and bind_function_type (ident : perkident) (typ : perktype) : unit =
  try
    let _ = Hashtbl.find fundecl_symbol_table ident in
    ()
  with Not_found -> Hashtbl.add fundecl_symbol_table ident typ

(** Transforms a perk program into a string containing the corresponding C
    program. *)
and codegen_program (tldfs : topleveldef_a list) : string =
  let s =
    say_here "codegen_program";
    filter_var_table ();
    (* List.iter
       (fun (id, typ) ->
         Printf.printf "%s: %s\n" id (type_descriptor_of_perktype typ))
       !all_vars; *)
    let body =
      String.concat "\n"
        (List.map
           (fun tldf ->
             let code = codegen_topleveldef tldf in
             if String.length code = 0 then "" else Printf.sprintf "%s\n" code)
           tldfs)
      |> String.trim
    in
    (* Write includes *)
    "#include <malloc.h>\n#include <string.h>\n#include <stdbool.h>\n"
    ^ String.concat "\n"
        (List.rev
           (List.map (fun lib -> Printf.sprintf "#include %s" lib) !import_list))
    ^ "\n\n"
    (* Write macros *)
    (* ^ "typedef struct env_ {} env_;\n" *)
    ^ "typedef struct _lambdummy_type {\n\
      \    void *env;\n\
      \    void *func;\n\
       } __lambdummy_type;\n\
       static __lambdummy_type *__lambdummy;\n\n\
      \ __lambdummy_type *alloclabmd(int size, void *labmda, void *env)\n\
       {\n\
      \    __lambdummy_type *ptr = malloc(sizeof(__lambdummy_type));\n\
      \    ptr->env = malloc(size);\n\
      \    memcpy(ptr->env, env, size);\n\
      \    ptr->func = labmda;\n\
      \    return ptr;\n\
       }"
    ^ "\n\n"
    ^ "#define CALL_LAMBDA0(l, t) (__lambdummy = (__lambdummy_type *)l, \
       ((t)(__lambdummy->func))())\n\
       #define CALL_LAMBDA(l, t, ...) (__lambdummy = (__lambdummy_type \
       *)l,       ((t)(__lambdummy->func))(__VA_ARGS__))" ^ "\n\n"
    ^ generate_types () ^ "\n"
    ^ Hashtbl.fold
        (fun id typ acc ->
          Printf.sprintf "%s%s;\n" acc (codegen_fundecl id typ))
        fundecl_symbol_table ""
    (* Write program code *)
    ^ "\n"
    ^ body ^ "\n"
    (* Write lambdas *)
    ^ Hashtbl.fold
        (fun _ v acc -> Printf.sprintf "%s\n%s\n" acc (snd_4 v))
        lambdas_hashmap ""
  in
  (* Printf.printf "dependenciesoftype called: %d\nused: %d\nunused: %d\n"
       !called_counter !used_counter !unused_counter;
     Printf.printf "resolve_type called: %d\nused: %d\nunused: %d\n" !resolve_count
       !resolve_hit !resolve_miss; *)
  s

(** Generates code for a top level definition. *)
and codegen_topleveldef (tldf : topleveldef_a) : string =
  (* try *)
  say_here (Printf.sprintf "codegen_topleveldef: %s" (show_topleveldef_a tldf));
  let indent_string = "" in
  match ( $ ) tldf with
  | Archetype (i, l) ->
      let _ = add_archetype i in
      (* Remove discrimination between function declarations and
         regular variable declarations as they are not relevant here *)
      let l =
        List.map
          (fun t ->
            match ( $ ) t with
            (* Add void pointer to declared functions: self *)
            | DeclFun (t, id) -> (add_parameter_to_func void_pointer t, id)
            | DeclVar (t, id) -> (t, id))
          l
      in
      List.iter
        (fun t ->
          let typ, id = t in
          add_binding_to_archetype i id typ)
        l;
      add_code_to_type_binding
        ([], ArcheType (i, l), [])
        (Printf.sprintf "\n%sstruct %s {\n%s\n};\ntypedef struct %s %s;\n"
           indent_string i
           (if List.length l = 0 then ""
            else
              (indent_string ^ "    "
              ^ String.concat
                  (";\n" ^ indent_string ^ "    ")
                  (List.map
                     (fun ((a, typ, d), id) ->
                       let typ =
                         (* Transform declarations into pointers:
                            at runtime, archetypes are structs of pointers *)
                         match typ with
                         | Funtype (_, _) | Lambdatype (_, _, _) -> (a, typ, d)
                         | _ -> ([], Pointertype (a, typ, d), [])
                       in
                       codegen_decl (typ, id))
                     l))
              ^ ";")
           i i);
      ""
  | Model (name, il, defs) ->
      let archetypes = List.map (fun n -> (n, get_archetype n)) il in
      let archetype_decls =
        List.map
          (fun (n, h) -> Hashtbl.fold (fun k v acc -> (v, k, n) :: acc) h [])
          archetypes
      in
      let archetype_decls = List.flatten archetype_decls in

      (* Add self to functions AND ONLY functions (not lambdas) *)
      let selftype = self_type name in
      let defs =
        List.map
          (fun def ->
            match ( $ ) def with
            | DefFun (attrs, (typ, id, params, cmd)) ->
                annot_copy def
                  (DefFun
                     ( attrs,
                       ( add_parameter_to_func selftype typ,
                         id,
                         (selftype, "self") :: params,
                         cmd ) ))
            | DefVar (attrs, ((typ, id), expr)) -> (
                match (typ, ( $ ) expr) with
                | (_, Funtype (_, _), _), Lambda (_, _, _, _) ->
                    raise_type_error expr
                      "impossible: free vars should be empty in funtype"
                | _ -> annot_copy def (DefVar (attrs, ((typ, id), expr)))))
          defs
      in

      (* Find constructor function *)
      let constructor =
        List.find_opt
          (fun def ->
            match ( $ ) def with
            | DefFun (_, (_, id, _, _)) -> id = "constructor"
            | DefVar (_, ((_, id), expr)) ->
                if id = "constructor" then
                  raise_type_error expr "Constructor cannot be a lambda"
                else false)
          defs
        |> Option.map ( $ ) (* NDR: remove annotation *)
      in

      (* Generate strings for constructor parameters *)
      let params_str_with_types, params_str, params_typ =
        match constructor with
        | None -> ("", "", [])
        | Some (DefFun (_, (_typ, _, params, _))) ->
            let params =
              try List.tl params
              with Failure _ ->
                raise_type_error tldf
                  "constructor has 0 arguments. If you see this error, please \
                   open an issue at https://github.com/Alex23087/Perk"
            in
            let params = List.map fst params in
            ( String.concat ", "
                (List.mapi
                   (fun (i : int) (t : perktype) ->
                     Printf.sprintf "%s arg_%d" (codegen_type t) i)
                   params),
              String.concat ", "
                (List.mapi (fun i _ -> Printf.sprintf "arg_%d" i) params),
              params )
        | _ ->
            raise_compilation_error tldf
              "Impossible: constructor is not a function. This should not \
               happen"
      in

      (* Discard information about function/not-function definition *)
      let mems = List.map decl_of_deforfun defs in

      (* Gather member functions *)
      let member_funcs = get_member_functions defs in
      (* Generate member-containing struct *)
      add_code_to_type_binding
        ([], Modeltype (name, il, mems, params_typ, member_funcs), [])
        (Printf.sprintf "\n%sstruct %s {\n%s%s\n};\n%stypedef struct %s* %s;\n"
           indent_string name
           (if List.length mems = 0 then ""
            else
              (indent_string ^ "    "
              ^ String.concat
                  (";\n" ^ indent_string ^ "    ")
                  (List.map codegen_decl (List.map snd mems)))
              ^ ";")
           (if List.length il = 0 then ""
            else
              "\n\n"
              ^ String.concat "\n"
                  (List.map
                     (fun s ->
                       Printf.sprintf "%sstruct %s %s;" (indent_string ^ "    ")
                         s s)
                     il))
           indent_string name name);

      (* Generate function to create model *)
      let params_str = if params_str = "" then "" else ", " ^ params_str in
      let constructor_call_str =
        if Option.is_none constructor then ""
        else
          Printf.sprintf "%s    self->constructor(self%s);\n" indent_string
            params_str
      in
      (* Generate string containing initialization of the model's members *)
      let initializers_str =
        if List.length mems = 0 then ""
        else
          indent_string ^ "    "
          ^ String.concat
              (";\n" ^ indent_string ^ "    ")
              (List.map
                 (fun def ->
                   match ( $ ) def with
                   | DefFun (_, func) ->
                       (* Synthesize lambda for more homogeneous treatment 👍 *)
                       let _typ, id, _params, _cmd = func in
                       let typ = funtype_of_perkfundef func in
                       let lambda = annot_copy def (lambda_of_func func) in
                       Printf.sprintf "self->%s = (%s) %s" id (codegen_type typ)
                         (codegen_functional ~is_lambda:false lambda)
                   | DefVar (_, ((typ, id), expr)) -> (
                       let expr_str, letindefs =
                         codegen_expr_and_letindefs expr
                       in
                       match ( $ ) expr with
                       | Array _ ->
                           (* Workaround for array initialization in models: Create a fresh array and copy it to the model *)
                           let fv = fresh_var "array_init" in
                           let cdg_type = codegen_type typ in
                           Printf.sprintf
                             "%s\n\
                              %s    %s %s = %s;\n\
                              %s    memcpy(&self->%s, &%s, sizeof(%s))"
                             letindefs indent_string cdg_type fv expr_str
                             indent_string id fv fv
                       | _ ->
                           Printf.sprintf "%s\n%s    self->%s = (%s) %s"
                             letindefs indent_string id (codegen_type typ)
                             expr_str))
                 defs)
          ^ ";"
      in
      (* Generate string containing initializations of the archetypes substructs *)
      let archetypes_substruct_str =
        if List.length archetype_decls == 0 then ""
        else
          let archetype_ident_type =
            List.flatten
              (List.map
                 (fun (i, h) ->
                   Hashtbl.fold (fun k v acc -> (i, k, v) :: acc) h [])
                 archetypes)
          in
          String.concat ";\n"
            (List.map
               (fun (a, id, t) ->
                 match t with
                 (* Functions don't need to be referenced, as they are already pointers *)
                 | _a, Funtype _, _q ->
                     Printf.sprintf "%s    self->%s.%s = (%s) self->%s"
                       indent_string a id (codegen_type t) id
                 | _a, Lambdatype (_params, _retype, _free_vars), _q ->
                     raise_type_error tldf
                       "lambdas not yet supported in models 2"
                 | _ ->
                     Printf.sprintf "%s    self->%s.%s = (%s*) &self->%s"
                       indent_string a id (codegen_type t) id)
               archetype_ident_type)
          ^ ";"
      in
      (* Construct the initializer definition using the previously defined strings *)
      Printf.sprintf
        "%s%s %s_init(%s) {\n\
        \    %s%s self = malloc(sizeof(struct %s));\n\
         %s\n\
         %s\n\
         %s    %sreturn self;\n\
         }"
        indent_string name name params_str_with_types indent_string name name
        initializers_str archetypes_substruct_str constructor_call_str
        indent_string
  | Struct (id, fields) ->
      let fields_decl = List.map fst fields in
      add_code_to_type_binding
        ([], Structtype (id, fields), [])
        (Printf.sprintf "\n%stypedef struct %s {\n%s\n} %s;\n" indent_string id
           (if List.length fields_decl = 0 then ""
            else
              (indent_string ^ "    "
              ^ String.concat
                  (";\n" ^ indent_string ^ "    ")
                  (List.map codegen_decl fields_decl))
              ^ ";")
           id);
      ""
  | ADT (id, constructors) ->
      let adt_type = ([], AlgebraicType (id, constructors), []) in
      let adt_desc = type_descriptor_of_perktype adt_type in
      add_code_to_type_binding adt_type
        (Printf.sprintf
           "\n\
            %stypedef enum __perk_%s_Tag {\n\
            %s\n\
            %s} __perk_%s_Tag;\n\n\
            %stypedef struct %s {\n\
           \    %s\n\
            %s} %s;\n"
           indent_string id
           (String.concat ",\n"
              (List.map
                 (fun (c, _) ->
                   Printf.sprintf "%s    __perk_%s_Tag" indent_string c)
                 constructors))
           indent_string id indent_string id
           (Printf.sprintf "__perk_%s_Tag tag;\n    union {\n%s\n    } data;" id
              (String.concat "\n"
                 (List.map
                    (fun (c, t) ->
                      Printf.sprintf
                        "%s        struct {\n            %s;\n        } %s;"
                        indent_string
                        (String.concat ";\n            "
                           (List.mapi
                              (fun i t -> Printf.sprintf "%s* _%d" t i)
                              t))
                        c)
                    (List.filter_map
                       (fun (_c, t) ->
                         match t with
                         | [] -> None
                         | _ -> Some (_c, List.map codegen_type t))
                       constructors))))
           indent_string id);
      List.iter
        (fun (c, (t : perktype list)) ->
          bind_function_type c ([], Funtype (t, adt_type), []);

          Hashtbl.add lambdas_hashmap (annotate_dummy (Int 1))
            ( c,
              (match t with
              | [] ->
                  Printf.sprintf "%s %s() {\n    %s\n}" adt_desc c
                    (Printf.sprintf
                       "%s out;\n    out.tag = __perk_%s_Tag;\n    return out;"
                       adt_desc c)
              | _ ->
                  Printf.sprintf "%s %s(%s) {\n    %s\n}" adt_desc c
                    (String.concat ", "
                       (List.mapi
                          (fun i t ->
                            Printf.sprintf "%s arg_%d" (codegen_type t) i)
                          t))
                    (Printf.sprintf
                       "%s out;\n\
                       \    out.tag = __perk_%s_Tag;\n\
                       \    %s\n\
                       \    return out;"
                       adt_desc c
                       (String.concat "\n    "
                          (List.mapi
                             (fun i t ->
                               Printf.sprintf
                                 "out.data.%s._%d = malloc(sizeof(%s));\n\
                                 \    *out.data.%s._%d = arg_%d;"
                                 c i (codegen_type t) c i i)
                             t)))),
              [],
              type_descriptor_of_perktype
                ([], Funtype (List.map pointer_of_type t, adt_type), []) )
          |> ignore)
        constructors;
      ""
  | InlineC s -> s
  | Fundef (t, id, args, body) -> indent_string ^ codegen_fundef t id args body
  | Extern _ -> ""
  (* Externs are only useful for type checking. No need to keep it for codegen step *)
  | Def ((t, e), deftype) ->
      indent_string ^ codegen_def t e deftype indent_string
  | Import lib ->
      add_import lib;
      ""
  | Open _ ->
      raise_compilation_error tldf
        "Opens should not reach this point (codegen). If you see this error, \
         please open an issue at https://github.com/Alex23087/Perk/issues"
(* with Not_inferred s -> raise_type_error tldf s *)

(** Generates code for perk commands. *)
and codegen_command (cmd : command_a) (indentation : int) : string =
  let indent_string = String.make (4 * indentation) ' ' in
  let indent_maybe s = if String.length s != 0 then indent_string else "" in
  match ( $ ) cmd with
  | InlineCCmd s -> s
  | Block c ->
      indent_string ^ "{\n"
      ^ codegen_command c (indentation + 1)
      ^ "\n" ^ indent_string ^ "}"
  | DefCmd ((t, e), deftype) -> codegen_def t e deftype indent_string
  | Assign (l, r, lass_type, _rass_type) ->
      (* Printf.printf "Assignment type: %s\n"
         (match lass_type with Some t -> show_perktype t | None -> "None"); *)
      let expr_strr, letindefsr = codegen_expr_and_letindefs r in
      let expr_strl, letindefsl = codegen_expr_and_letindefs l in
      (Printf.sprintf "%s%s%s%s%s%s = %s;" (indent_maybe letindefsr) letindefsl
         (indent_maybe letindefsr) letindefsr indent_string
         (match lass_type with
         | Some (_, ArchetypeSum _, _) | Some (_, ArcheType _, _) ->
             Printf.sprintf "%s" expr_strl
         | _ -> Printf.sprintf "%s" expr_strl))
        (match _rass_type with
        | Some (_, Optiontype _t, _) ->
            Printf.sprintf "((%s){1, %s})"
              (codegen_type (Option.get _rass_type))
              expr_strr
        | _ -> expr_strr)
  | Seq (c1, c2) ->
      let c1_code = codegen_command c1 indentation in
      let c2_code = codegen_command c2 indentation in
      if String.length c1_code = 0 then Printf.sprintf "%s" c2_code
      else if String.length c2_code = 0 then Printf.sprintf "%s" c1_code
      else Printf.sprintf "%s\n%s" c1_code c2_code
  | IfThenElse (e, c1, c2) ->
      let expr_str, letindefs = codegen_expr_and_letindefs e in
      Printf.sprintf "%s%s%sif (%s) {\n%s\n%s} else {\n%s\n%s}"
        (indent_maybe letindefs) letindefs indent_string expr_str
        (codegen_command c1 (indentation + 1))
        indent_string
        (codegen_command c2 (indentation + 1))
        indent_string
  | Whiledo (e, c) ->
      let expr_str, letindefs = codegen_expr_and_letindefs e in
      Printf.sprintf "%s%s%swhile (%s) {\n%s\n%s}" (indent_maybe letindefs)
        letindefs indent_string expr_str
        (codegen_command c (indentation + 1))
        indent_string
  | Dowhile (e, c) ->
      let expr_str, letindefs = codegen_expr_and_letindefs e in
      Printf.sprintf "%s%s%sdo {\n%s\n%s} while (%s);" (indent_maybe letindefs)
        letindefs indent_string
        (codegen_command c (indentation + 1))
        indent_string expr_str
  | For (c1, e2, c3, body) ->
      let c1_code = codegen_command c1 0 in
      let c1_code =
        if String.ends_with c1_code ~suffix:";" then
          String.sub c1_code 0 (String.length c1_code - 1)
        else c1_code
      in
      let c3_code = codegen_command c3 0 in
      let c3_code =
        if String.ends_with c3_code ~suffix:";" then
          String.sub c3_code 0 (String.length c3_code - 1)
        else c3_code
      in
      let expr_str, letindefs = codegen_expr_and_letindefs e2 in
      Printf.sprintf "%s%s%sfor (%s; %s; %s) {\n%s\n%s}"
        (indent_maybe letindefs) letindefs indent_string c1_code expr_str
        c3_code
        (codegen_command body (indentation + 1))
        indent_string
  | Expr e ->
      let expr_str, letindefs = codegen_expr_and_letindefs e in
      Printf.sprintf "%s%s%s%s;" (indent_maybe letindefs) letindefs
        indent_string expr_str
  | Skip -> ""
  | Switch (e, cases) ->
      let letindefs_cases, cases_str =
        let both =
          List.map
            (fun (e, c) ->
              let expr_str, letindefs = codegen_expr_and_letindefs e in
              ( indent_string ^ "    "
                ^ Printf.sprintf "case %s: {\n%s\n}" expr_str
                    (codegen_command c (indentation + 1)),
                letindefs ))
            cases
        in
        ( List.map snd both |> String.concat "\n",
          List.map fst both |> String.concat "\n" )
      in
      let expr_str, letindefs = codegen_expr_and_letindefs e in
      Printf.sprintf "%s%s%sswitch (%s) {%s\n%s}"
        (indent_maybe (letindefs ^ letindefs_cases))
        (letindefs ^ letindefs_cases)
        indent_string expr_str cases_str indent_string
  | Banish name ->
      (* TODO: Automatically banish children (possibly add autobanish keyword to models members) *)
      Printf.sprintf "%sfree(%s);\n%s%s = NULL;" indent_string name
        indent_string name
  | Return None -> indent_string ^ Printf.sprintf "return;"
  | Return (Some e) ->
      let expr_str, letindefs = codegen_expr_and_letindefs e in
      Printf.sprintf "%s%s%sreturn %s;" (indent_maybe letindefs) letindefs
        indent_string expr_str
  | Continue -> indent_string ^ Printf.sprintf "continue;"
  | Break -> indent_string ^ Printf.sprintf "break;"

(** Generates code for perk definitions. *)
and codegen_def (t : perkvardesc) (e : expr_a) (deftype : perktype option)
    (indent_string : string) : string =
  let indent_maybe s = if String.length s != 0 then indent_string else "" in
  let decl_str = codegen_decl t in
  let expr_str, letindefs = codegen_expr_and_letindefs e in
  match deftype with
  | Some (_, Optiontype _, _) ->
      Printf.sprintf "%s%s%s%s = {1, %s};" (indent_maybe letindefs) letindefs
        indent_string decl_str expr_str
  | _ ->
      Printf.sprintf "%s%s%s%s = %s;" (indent_maybe letindefs) letindefs
        indent_string decl_str expr_str

(** Generates code for perk declaration. *)
and codegen_decl (t : perkvardesc) : string =
  let t, id = t in
  let type_str = codegen_type t in
  Printf.sprintf "%s %s" type_str id

(** Generates code for perk function definitions. *)
and codegen_fundef (t : perktype) (id : perkident) (args : perkvardesc list)
    (body : command_a) : string =
  let type_str = codegen_type t in
  let args_str =
    String.concat ", "
      (List.map
         (fun (t, id) ->
           Printf.sprintf "%s %s" (codegen_type (resolve_type t)) id)
         args)
  in
  let body_str = codegen_command body 1 in
  let funtype = ([], Funtype (List.map (fun (t, _) -> t) args, t), []) in
  bind_function_type id funtype;
  Printf.sprintf "%s %s(%s) {\n%s\n}" type_str id args_str body_str

(**transforms a perktype into a C type *)
(* TODO figure out details (attrs, quals)...... *)
and codegen_type ?(expand : bool = false) (t : perktype) : string =
  (* Printf.printf "codegen_type: %s\n" (show_perktype t); flush stdout; *)
  let attrs, t', quals = t in
  let attrs_str = String.concat " " (List.map codegen_attr attrs) in
  let quals_str = String.concat " " (List.map codegen_qual quals) in
  let type_str =
    match t' with
    | Basetype s -> s
    | Structtype (id, _) -> id
    | AlgebraicType (id, _constructors) -> id
    | Funtype _ -> type_descriptor_of_perktype t
    | Lambdatype _ -> type_descriptor_of_perktype t
    | Pointertype ([], Structtype _, _) when expand -> "void*"
    | Pointertype t -> Printf.sprintf "%s*" (codegen_type t ~expand)
    | Arraytype (_at, _n) -> type_descriptor_of_perktype t
    | Vararg -> "..."
    | Modeltype (name, _archs, _decls, _constr_params, _member_funcs) ->
        if expand then "void*" else name
    | ArcheType (name, _decls) -> name
    | ArchetypeSum _archs -> type_descriptor_of_perktype t
    | Infer ->
        raise
          (Not_inferred "Impossible: type has not been inferred in codegen_type")
    | Optiontype _t -> type_descriptor_of_perktype t
    | Tupletype _ts -> type_descriptor_of_perktype t
  in
  if attrs_str = "" && quals_str = "" then type_str
  else if attrs_str = "" then Printf.sprintf "%s %s" quals_str type_str
  else if quals_str = "" then Printf.sprintf "%s %s" attrs_str type_str
  else Printf.sprintf "%s %s %s" attrs_str quals_str type_str

(** Codegens perk attributes. *)
and codegen_attr (attr : perktype_attribute) : string =
  match attr with Public -> "" | Private -> "static" | Static -> "static"

(** Codegens perk qualifiers. *)
and codegen_qual (qual : perktype_qualifier) : string =
  match qual with
  | Const -> "const"
  | Volatile -> "volatile"
  | Restrict -> "restrict"

(** Codegens perk expressions (first output string), also returning the code
    that declares the fresh variables used in generating let...in-like
    expressions (second output string). *)
and codegen_expr_and_letindefs (e : expr_a) : string * string =
  generated_freevars := "";
  let expr_gend = codegen_expr e in
  let res = (expr_gend, !generated_freevars) in
  generated_freevars := "";
  res

(** Codegen expressions. *)
and codegen_expr (e : expr_a) : string =
  let e' = ( $ ) e in
  match e' with
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Char c -> Printf.sprintf "'%c'" c
  | String s -> Printf.sprintf "\"%s\"" (String.escaped s)
  | Var id -> id
  | Apply (e, args, _app_type) -> (
      (* Printf.printf "Applying lambda with type %s\n"
         (match _app_type with
         | None -> "None"
         | Some t -> type_descriptor_of_perktype t); *)
      let expr_str = codegen_expr e in
      let args_str = String.concat ", " (List.map codegen_expr args) in
      match _app_type with
      | None -> (
          match ( $ ) e with
          | Access (e1, id, acctype, _) -> (
              let e1_str = codegen_expr e1 in
              let args_str =
                if List.length args = 0 then "" else ", " ^ args_str
              in
              match Option.map resolve_type acctype with
              | Some ((_, Modeltype _, _) as typ) ->
                  let fresh_ide = fresh_var "model_appl" in
                  let e1_decl_string =
                    Printf.sprintf "%s %s = %s;\n" (codegen_type typ) fresh_ide
                      e1_str
                  in
                  generated_freevars := !generated_freevars ^ e1_decl_string;
                  Printf.sprintf "%s -> %s (%s%s)" fresh_ide id fresh_ide
                    args_str
              | Some _t ->
                  Printf.sprintf "%s(%s.self%s)" expr_str e1_str
                    args_str (* this is for archetype sums *)
              | None ->
                  raise_compilation_error e "Impossible: no acctype for access"
              (* this is for models *))
          | _ -> Printf.sprintf "%s(%s)" expr_str args_str)
      | Some lamtype -> (
          match ( $ ) e with
          | Access (e1, ide, acctype, _) -> (
              let e1_str = codegen_expr e1 in
              let args_str =
                if List.length args = 0 then "" else ", " ^ args_str
              in
              match Option.map resolve_type acctype with
              (* this is for models *)
              (* Case: member function *)
              | Some (_, Modeltype (_n, _, _, _, member_funcs), _)
                when List.mem ide member_funcs ->
                  let lamtype = add_parameter_to_func (self_type _n) lamtype in
                  let lamtype_desc =
                    type_descriptor_of_perktype (func_of_lambda_void lamtype)
                  in
                  let args_str = e1_str ^ args_str in
                  Printf.sprintf "CALL_LAMBDA(%s, %s, %s)" expr_str lamtype_desc
                    args_str
              (* Case: non-member function *)
              | Some (_, Modeltype (_n, _, _, _, _), _) ->
                  let lamtype_desc =
                    type_descriptor_of_perktype (functype_of_lambdatype lamtype)
                  in
                  let args_str = args_str in
                  Printf.sprintf "CALL_LAMBDA%s(%s, %s%s)"
                    (if List.length args > 0 then "" else "0")
                    expr_str lamtype_desc args_str
              | Some (_, ArcheType (_name, _), _) ->
                  let lamtype = add_parameter_to_func void_pointer lamtype in
                  let lamtype_desc =
                    type_descriptor_of_perktype (func_of_lambda_void lamtype)
                  in
                  let args_str = e1_str ^ ".self" ^ args_str in
                  Printf.sprintf "CALL_LAMBDA(%s, %s, %s)" expr_str lamtype_desc
                    args_str
                  (* this is for archetype sums *)
              | Some t ->
                  failwith
                    (Printf.sprintf
                       "Impossible: no acctype for access: got type %s"
                       (show_perktype t))
              | None -> failwith "Impossible: no acctype for access: got None")
          | _ ->
              let args_str =
                if List.length args = 0 then "" else ", " ^ args_str
              in
              if List.length args = 0 then
                Printf.sprintf "CALL_LAMBDA0(%s, %s)" expr_str
                  (type_descriptor_of_perktype (functype_of_lambdatype lamtype))
              else
                Printf.sprintf "CALL_LAMBDA(%s, %s%s)" expr_str
                  (type_descriptor_of_perktype (functype_of_lambdatype lamtype))
                  args_str))
  | Access (e1, ide, acctype, rightype) -> (
      match Option.map resolve_type acctype with
      | Some (_, Modeltype _, _) ->
          Printf.sprintf "%s->%s" (codegen_expr e1) ide
      | Some (_, Arraytype (_t, Some n), _) -> string_of_int n
      | Some (_, Structtype (_id, _), _) ->
          Printf.sprintf "%s.%s" (codegen_expr e1) ide
      | Some t -> (
          match Option.map discard_type_aq rightype with
          | Some (Lambdatype _) | Some (Funtype _) ->
              Printf.sprintf "%s.%s.%s" (codegen_expr e1)
                (type_descriptor_of_perktype t)
                ide
          | Some _ | None ->
              Printf.sprintf "(*(%s.%s.%s))" (codegen_expr e1)
                (type_descriptor_of_perktype t)
                ide)
      | None -> failwith "Impossible: no acctype for access")
  | Binop (op, e1, e2) ->
      Printf.sprintf "%s %s %s" (codegen_expr e1) (codegen_binop op)
        (codegen_expr e2)
  | PreUnop (op, e) ->
      Printf.sprintf "%s%s" (codegen_preunop op) (codegen_expr e)
  | PostUnop (op, e) -> (
      match op with
      | OptionGet (Some t) ->
          Printf.sprintf "((%s)%s%s)" (c_type_of_perktype t) (codegen_expr e)
            (codegen_postunop op)
      | OptionGet None -> raise_type_error e "Option get type was not inferred"
      | _ -> Printf.sprintf "%s%s" (codegen_expr e) (codegen_postunop op))
  | Parenthesised e -> Printf.sprintf "(%s)" (codegen_expr e)
  | Lambda _ -> codegen_functional ~is_lambda:true e
  | Subscript (e1, e2) ->
      Printf.sprintf "%s[%s]" (codegen_expr e1) (codegen_expr e2)
  | Summon (typident, args) ->
      let args_str = String.concat ", " (List.map codegen_expr args) in
      (* Printf.sprintf "%sstruct %s %s;\n%s%s_init(&%s);\n%s%s.constructor((void*)&%s, %s);" indent_string typident name indent_string typident name indent_string name name args_str *)
      Printf.sprintf "%s_init(%s)" typident args_str
  | Tuple (es, typ) ->
      Printf.sprintf "(%s){%s}"
        (typ |> Option.get |> codegen_type)
        (String.concat ", "
           (List.map (fun e -> Printf.sprintf "%s" (codegen_expr e)) es))
  | TupleSubscript (e, i) -> Printf.sprintf "%s._%d" (codegen_expr e) i
  | As (expr, typlist, typ) ->
      if Option.is_none typ then
        raise_compilation_error e
          "The type for 'as' expression has not been tagged during typecheck. \
           If this happens, please open an issue at \
           https://github.com/Alex23087/Perk/issues";
      let typ = Option.get typ in
      let sum_type_descr = codegen_type ([], ArchetypeSum typlist, []) in
      let code = codegen_expr expr in
      let new_id = fresh_var "as_expr" in
      let new_decl =
        Printf.sprintf "%s %s = %s;\n" (codegen_type typ) new_id code
      in
      generated_freevars := !generated_freevars ^ new_decl;
      Printf.sprintf "((%s) {%s%s})" sum_type_descr
        (if List.length typlist = 0 then ""
         else
           String.concat ", "
             (List.map
                (fun t ->
                  (* Structs representing archetypes are accessed differently depending on
                  whether the variable is a model or an archetype sum *)
                  match typ with
                  | _, Modeltype _, _ ->
                      Printf.sprintf "%s->%s" new_id
                        (codegen_type t ~expand:true)
                  | _, ArchetypeSum _, _ ->
                      Printf.sprintf "%s.%s" new_id
                        (codegen_type t ~expand:true)
                  | _ ->
                      raise_compilation_error e
                        "as expression can only be used with models or \
                         archetypes")
                typlist)
           ^ ", ")
        (* If the variable is an archetype sum, the internal self pointer is passed *)
        (match typ with
        | _, Modeltype _, _ -> new_id
        | _, ArchetypeSum _, _ -> Printf.sprintf "%s.self" new_id
        | _ ->
            raise_compilation_error e
              "as expression can only be used with models or archetypes")
  | Nothing t -> (
      match t with
      | _, Infer, _ ->
          failwith "Impossible: type for nothing has not been inferred"
      | t -> Printf.sprintf "((%s) {0, 0})" (type_descriptor_of_perktype t))
  | Something (e, t) -> (
      match t with
      | _, Infer, _ ->
          failwith "Impossible: type for something has not been inferred"
      | t ->
          Printf.sprintf "((%s) {1, %s})"
            (type_descriptor_of_perktype ([], Optiontype t, []))
            (codegen_expr e))
  | Array es ->
      Printf.sprintf "{%s}"
        (String.concat ", "
           (List.map (fun e -> Printf.sprintf "%s" (codegen_expr e)) es))
  | Cast ((from_t, to_t), e) -> (
      match (from_t, to_t) with
      (* | (_, Funtype _, _), (_, Lambdatype _, _) ->
          (* raise_type_error e "cannot cast function to lambda yet" *)
          (* Printf.sprintf "((%s) {NULL, %s})"
            (type_descriptor_of_perktype to_t) *)
          codegen_expr e *)
      | (_, Lambdatype _, _), (_, Lambdatype _, _) ->
          Printf.sprintf "((%s)%s)"
            (type_descriptor_of_perktype to_t)
            (codegen_expr e)
      | _ ->
          Printf.sprintf "((%s)(%s))" (c_type_of_perktype to_t) (codegen_expr e)
      )
  | IfThenElseExpr (guard, then_e, else_e) ->
      Printf.sprintf "(%s ? %s : %s)" (codegen_expr guard) (codegen_expr then_e)
        (codegen_expr else_e)
  | Make (id, inits) ->
      let fields =
        match lookup_type id with
        | Some (_, Structtype (_, fields), _) -> fields
        | _ ->
            raise_type_error e
              (Printf.sprintf "There is no struct with name %s" id)
      in
      Printf.sprintf "{%s}"
        (String.concat ", "
           (List.map
              (fun ((_typ, field), expr) ->
                let initialiser =
                  List.find_opt (fun (f, _) -> f = field) inits
                in
                match initialiser with
                | Some (_, e) -> codegen_expr e
                | None -> codegen_expr expr)
              fields))

(* struct {int is_empty; int value;} palle = {0,1}; *)

(** generates code for binary operators *)
and codegen_binop (op : binop) : string =
  match op with
  | Add -> "+"
  | Mul -> "*"
  | Div -> "/"
  | Sub -> "-"
  | Eq -> "=="
  | Lt -> "<"
  | Leq -> "<="
  | Gt -> ">"
  | Geq -> ">="
  | Land -> "&&"
  | Lor -> "||"
  | Neq -> "!="

(** generates code for prefix unary operators *)
and codegen_preunop (op : preunop) : string =
  match op with
  | Neg -> "-"
  | Not -> "!"
  | Dereference -> "*"
  | Reference -> "&"
  | PreIncrement -> "++"
  | PreDecrement -> "--"

(** generates code for postfix unary operators *)
and codegen_postunop (op : postunop) : string =
  match op with
  | PostIncrement -> "++"
  | PostDecrement -> "--"
  | OptionIsSome -> ".is_some"
  | OptionGet _ -> ".contents"

(** generates code for function declarations *)
and codegen_fundecl (id : perkident) (typ : perktype) : string =
  let attrs, t, _ = typ in
  let attrs_str = String.concat " " (List.map codegen_attr attrs) in
  let type_str =
    match t with
    | Funtype (args, ret) | Lambdatype (args, ret, _) ->
        codegen_type ret ^ " " ^ id ^ " ("
        ^ String.concat ", " (List.map codegen_type args)
        ^ ")"
    | _ -> failwith "codegen_fundecl: called with a non function type"
  in
  if attrs_str = "" then type_str else Printf.sprintf "%s %s" attrs_str type_str

and generate_types () =
  let out = ref "" in
  let ft_list =
    ref
      (Hashtbl.fold
         (fun k (typ, code) acc ->
           if is_builtin_type k (typ, code) then acc
           else (
             out := !out ^ generate_forward_declaration typ;
             (k, (typ, code, dependencies_of_type typ)) :: acc))
         type_symbol_table [])
  in

  let sort_based_on_deps_count (_, (_, _, d_a)) (_, (_, _, d_b)) =
    compare (List.length d_a) (List.length d_b)
  in
  ft_list := List.sort sort_based_on_deps_count !ft_list;
  List.iter
    (fun (id, (_, _, deps)) ->
      say_here
        (Printf.sprintf "Type: %s, Dependencies: [%s]\n" id
           (String.concat ", " deps)))
    !ft_list;
  while List.length !ft_list > 0 do
    (* say_here "generate_types"; *)
    let _id, (_typ, _code, _deps) = List.hd !ft_list in
    (* if List.length _deps > 0 then *)
    (* Printf.printf "%s\n"
       (Printf.sprintf "Warning: Type %s depends on %s, not yet generated\n"
          (List.hd (List.map fst !ft_list))
          (List.hd
             (List.map (fun (_, (_, _, d)) -> String.concat ", " d) !ft_list))); *)
    let _code = Some (codegen_type_definition _typ) in
    ft_list := List.tl !ft_list;
    (* Remove dzpendency from other elements *)
    ft_list :=
      List.sort sort_based_on_deps_count
        (List.map
           (fun (_i, (_typ, _code, _deps)) ->
             (_i, (_typ, _code, List.filter (fun id -> id <> _id) _deps)))
           !ft_list);
    match _code with
    | Some c -> out := if c = "" then !out else Printf.sprintf "%s%s\n" !out c
    | None -> ()
  done;
  !out

(** Generates code for type definitions. *)
and codegen_type_definition (t : perktype) : string =
  let key = type_descriptor_of_perktype t in
  let _t, _code = Hashtbl.find type_symbol_table key in
  match _code with
  (* Some types (e.g., Models, Archetypes) will have code already generated *)
  | Some c -> c
  | None -> (
      say_here (Printf.sprintf "codegen_type_definition: %s\n" key);
      match t with
      | _, Tupletype ts, _ ->
          let type_str = type_descriptor_of_perktype t in
          let compiled =
            Printf.sprintf "typedef struct %s {%s} %s;" type_str
              (if List.length ts = 0 then ""
               else
                 String.concat "; "
                   (List.mapi
                      (fun i t -> Printf.sprintf "%s _%d" (codegen_type t) i)
                      ts)
                 ^ ";")
              type_str
          in
          Hashtbl.replace type_symbol_table key (_t, Some compiled);
          compiled
      | _, Optiontype u, _ ->
          let u = resolve_type u in
          let compiled =
            Printf.sprintf "typedef struct %s {int is_some; %s contents;} %s;"
              key
              (match u with
              | _, Modeltype _, _ -> "void*"
              | _, Pointertype typ, _ ->
                  type_descriptor_of_perktype typ ^ "*"
                  (* TODO REMOVE THIS HORRIBLE FIX (is it horrible though?) *)
              | _ -> type_descriptor_of_perktype u)
              key
          in
          Hashtbl.replace type_symbol_table key (_t, Some compiled);
          compiled
      | _, Funtype (args, ret), _ ->
          let type_str = type_descriptor_of_perktype t in
          let typedef_str =
            Printf.sprintf "typedef %s"
              (codegen_type ret ~expand:true
              ^ " (*" ^ type_str ^ ")("
              ^ String.concat ", "
                  (List.map (fun t -> codegen_type t ~expand:true) args)
              ^ ");")
          in
          Hashtbl.replace type_symbol_table key (t, Some typedef_str);
          (* Hashtbl.add function_type_hashmap t (type_str, typedef_str, expanded_str); *)
          typedef_str
      | _, Pointertype t, _ ->
          Printf.sprintf "typedef %s* %s;" (codegen_type t ~expand:true) key
      | _, ArchetypeSum archs, _ ->
          let compiled =
            Printf.sprintf "struct %s {%svoid* self;};\ntypedef struct %s %s;"
              (type_descriptor_of_perktype t)
              (if List.length archs = 0 then ""
               else
                 String.concat "; "
                   (List.map
                      (fun t ->
                        Printf.sprintf "%s %s"
                          (type_descriptor_of_perktype t)
                          (type_descriptor_of_perktype t))
                      archs)
                 ^ "; ")
              (type_descriptor_of_perktype t)
              (type_descriptor_of_perktype t)
          in
          Hashtbl.replace type_symbol_table key (_t, Some compiled);
          compiled
      | _, Arraytype (at, n), _ ->
          let compiled =
            match n with
            | Some n ->
                Printf.sprintf "typedef %s %s[%d];" (c_type_of_perktype at) key
                  n
            | None ->
                Printf.sprintf "typedef %s %s[];" (c_type_of_perktype at) key
          in
          Hashtbl.replace type_symbol_table key (_t, Some compiled);
          compiled
      | _, Lambdatype _, _ -> codegen_lambda_capture t
      | _ ->
          raise_type_error
            (annotate_dummy ([], Int (-1), []))
            (Printf.sprintf
               "Unexpected type generation request: got %s. If you see this \
                error, please file an issue at https://github.com/"
               (type_descriptor_of_perktype t)))

and codegen_lambda_environment (free_vars : perkvardesc list) :
    bool * string * string =
  let environment_type_desc, environment_type_desc_erased =
    ( type_descriptor_of_environment ~erase_env:false free_vars,
      type_descriptor_of_environment ~erase_env:true free_vars )
  in
  say_here (Printf.sprintf "Environment type: %s\n" environment_type_desc);
  let tmp_typedef =
    List.find_opt (fun (t, _) -> t = environment_type_desc) !lambda_environments
  in
  match tmp_typedef with
  | Some _typedef -> (false, environment_type_desc_erased, "")
  | None ->
      let environment_typedef =
        Printf.sprintf "typedef struct %s {%s;} %s;" environment_type_desc
          (String.concat "; "
             (List.mapi
                (fun i (typ, _id) ->
                  let typ =
                    match resolve_type typ with
                    (* | _, ArcheType (name, _), _ -> name *)
                    | _, Modeltype _, _ -> "void*"
                    | _ -> c_type_of_perktype typ
                  in
                  Printf.sprintf "%s _%d" typ i)
                free_vars))
          environment_type_desc
      in
      lambda_environments :=
        (environment_type_desc, environment_typedef) :: !lambda_environments;
      (true, environment_type_desc_erased, environment_typedef)

and codegen_lambda_capture (lamtype : perktype) : string =
  match lamtype with
  | _, Lambdatype (_retype, _args, free_vars), _ -> (
      let lambda_type_desc =
        type_descriptor_of_perktype (functype_of_lambdatype lamtype)
      in
      let env_needs_generation, environment_type_desc, environment_typedef =
        codegen_lambda_environment free_vars
      in
      let capture_type_desc = lambda_type_desc ^ "_" ^ environment_type_desc in
      let tmp_typedef =
        List.find_opt
          (fun (t, _) -> t = capture_type_desc)
          !lambda_capture_dummies
      in
      match tmp_typedef with
      | Some str -> snd str
      | None ->
          let typedef =
            Printf.sprintf
              "struct %s {\n\
              \    void* env;\n\
              \    %s func;\n\
               };\n\
               typedef struct %s* %s"
              capture_type_desc lambda_type_desc capture_type_desc
              capture_type_desc
          in
          let alltogether =
            Printf.sprintf "%s%s"
              (if env_needs_generation then environment_typedef else "")
              (if List.mem typedef !lambda_typedefs then ""
               else (
                 lambda_typedefs := typedef :: !lambda_typedefs;
                 "\n" ^ typedef ^ ";"))
          in
          (* Printf.printf "%s\n\n\n\n\n\n" alltogether; *)
          (* lambda_capture_dummies :=
            (capture_type_desc, alltogether) :: !lambda_capture_dummies; *)
          alltogether)
  | _ ->
      failwith
        "Impossible: codegen_lambda_capture: not a lambda expression. If you \
         see this error please file an issue at \
         https://github.com/Alex23087/Perk/issues"

and generate_forward_declaration (t : perktype) : string =
  match discard_type_aq t with
  | Tupletype _ ->
      let type_str = type_descriptor_of_perktype t in
      Printf.sprintf "typedef struct %s %s;\n" type_str type_str
  | AlgebraicType (id, _constructors) ->
      let type_str = type_descriptor_of_perktype t in
      Printf.sprintf "typedef struct %s %s;\n" type_str id
  | _ -> ""
