open Ast

(** Polyfuns that were EVER defined (in any file) -- maps polyfun ids to their
    signature and type parameter*)
let global_polyfuns : (string, topleveldef_a) Hashtbl.t = Hashtbl.create 10

let rec subst_perkvardesc ((pt, piden) : perkvardesc) (placeholder : perktype)
    (actual : perktype) =
  let subst_maybe t = subst_type t placeholder actual in
  (subst_maybe pt, piden)

and subst_type (t : perktype) (placeholder : perktype) (actual : perktype) =
  let base_subst t = if t = placeholder then actual else t in
  let subst_pvd t = subst_perkvardesc t placeholder actual in
  let subst_e e = subst_type_expr e placeholder actual in
  let subst_def (pvd, e) = (subst_pvd pvd, subst_e e) in
  match t with
  | _, Basetype _, _ -> base_subst t
  | a, Funtype (tl, t), b ->
      (a, Funtype (List.map base_subst tl, base_subst t), b)
  | a, Lambdatype (tl, t, pvl), b ->
      ( a,
        Lambdatype (List.map base_subst tl, base_subst t, List.map subst_pvd pvl),
        b )
  | a, Pointertype t, b -> (a, Pointertype (base_subst t), b)
  | a, Arraytype (t, io), b -> (a, Arraytype (base_subst t, io), b)
  | a, Structtype (s, defl), b -> (a, Structtype (s, List.map subst_def defl), b)
  | a, ArcheType (id, decll), b ->
      (a, ArcheType (id, List.map subst_pvd decll), b)
  | a, Modeltype (id, idl, attr_and_decl_list, tl, idl1), b ->
      ( a,
        Modeltype
          ( id,
            idl,
            List.map (fun (a, d) -> (a, subst_pvd d)) attr_and_decl_list,
            List.map base_subst tl,
            idl1 ),
        b )
  | a, AlgebraicType (id, id_and_tl_list, t_param), b ->
      ( a,
        AlgebraicType
          ( id,
            List.map
              (fun (id, tl) ->
                (id, List.map (fun t -> subst_type t placeholder actual) tl))
              id_and_tl_list,
            Option.map (fun x -> subst_type x placeholder actual) t_param ),
        b )
  | a, Optiontype t, b -> (a, Optiontype (base_subst t), b)
  | a, Tupletype tl, b -> (a, Tupletype (List.map base_subst tl), b)
  | a, ArchetypeSum tl, b -> (a, ArchetypeSum (List.map base_subst tl), b)
  | a, PolyADTPlaceholder (i, t), b ->
      (a, PolyADTPlaceholder (i, base_subst t), b)
  | _, Vararg, _ | _, Infer, _ -> t

and subst_perkdef (pvd, e) (placeholder : perktype) (actual : perktype) :
    perkdef =
  ( subst_perkvardesc pvd placeholder actual,
    subst_type_expr e placeholder actual )

and subst_mel (MatchCase (mc, eo, c)) (placeholder : perktype)
    (actual : perktype) =
  let subst_e = fun x -> subst_type_expr x placeholder actual in
  let subst_c = fun x -> subst_type_command x placeholder actual in
  let mc1 = subst_mc mc placeholder actual in
  MatchCase (mc1, Option.map subst_e eo, subst_c c)

and subst_mc (mc : match_case_a) (placeholder : perktype) (actual : perktype) :
    match_case_a =
  let subst_e = fun x -> subst_type_expr x placeholder actual in
  let subst_maybe t = subst_type t placeholder actual in
  annot_copy mc
    (match ( $ ) mc with
    | Matchall -> Matchall
    | MatchVar (id, t) -> MatchVar (id, Option.map subst_maybe t)
    | MatchExpr e -> MatchExpr (subst_e e)
    | CompoundCase (id, mc1) ->
        CompoundCase (id, List.map (fun x -> subst_mc x placeholder actual) mc1))

and subst_type_expr (e : expr_a) (placeholder : perktype) (actual : perktype) =
  let subst_e = fun x -> subst_type_expr x placeholder actual in
  let subst_maybe t = subst_type t placeholder actual in
  let subst_pvd pvd = subst_perkvardesc pvd placeholder placeholder in
  annot_copy e
    (match ( $ ) e with
    | Nothing t -> Nothing (subst_maybe t)
    | Something (e1, t) -> Something (subst_e e1, subst_maybe t)
    | Bool _ | Int _ | Float _ | Char _ | String _ | Var _ -> ( $ ) e
    | PolymorphicVar (id, t) -> PolymorphicVar (id, subst_maybe t)
    | Apply (e1, e1l, ret_t) ->
        Apply (subst_e e1, List.map subst_e e1l, Option.map subst_maybe ret_t)
    | Binop (op, e1, e2) -> Binop (op, subst_e e1, subst_e e2)
    | PreUnop (op, e1, typ) -> PreUnop (op, subst_e e1, typ)
    | Lambda (retype, args, body, free_variables, lambda_name) ->
        Lambda
          ( subst_maybe retype,
            List.map subst_pvd args,
            subst_type_command body placeholder actual,
            List.map subst_pvd free_variables,
            lambda_name )
    | PostUnop (op, e1) -> PostUnop (op, subst_e e1)
    | Parenthesised e1 -> Parenthesised (subst_e e1)
    | Subscript (e1, e2) -> Subscript (subst_e e1, subst_e e2)
    | TupleSubscript (e1, i) -> TupleSubscript (subst_e e1, i)
    | Summon (id, e1l) -> Summon (id, List.map subst_e e1l)
    | Make (id1, l) ->
        Make (id1, List.map (fun (id2, e1) -> (id2, subst_e e1)) l)
    | Access (e1, id, t1, t2) ->
        Access
          (subst_e e1, id, Option.map subst_maybe t1, Option.map subst_maybe t2)
    | Tuple (e1l, t1) -> Tuple (List.map subst_e e1l, Option.map subst_maybe t1)
    | As (e1, tl, t) ->
        As (subst_e e1, List.map subst_maybe tl, Option.map subst_maybe t)
    | Array el -> Array (List.map subst_e el)
    | Cast ((t1, t2), e1) -> Cast ((subst_maybe t1, subst_maybe t2), subst_e e1)
    | Sizeof t1 -> Sizeof (subst_maybe t1)
    | IfThenElseExpr (e1, e2, e3) ->
        IfThenElseExpr (subst_e e1, subst_e e2, subst_e e3))

and subst_type_command (c : command_a) (placeholder : perktype)
    (actual : perktype) =
  let subst_e = fun x -> subst_type_expr x placeholder actual in
  let subst_c = fun x -> subst_type_command x placeholder actual in
  let subst_maybe t = subst_type t placeholder actual in
  let subst_def d = subst_perkdef d placeholder actual in
  annot_copy c
    (match ( $ ) c with
    | InlineCCmd _ | Banish _ | Break | Continue | Skip -> ( $ ) c
    | DefCmd (def, t) -> DefCmd (subst_def def, Option.map subst_maybe t)
    | Block c1 -> Block (subst_c c1)
    | Assign (e1, e2, t1, t2) ->
        Assign
          ( subst_e e1,
            subst_e e2,
            Option.map subst_maybe t1,
            Option.map subst_maybe t2 )
    | Seq (c1, c2) -> Seq (subst_c c1, subst_c c2)
    | IfThenElse (e1, c1, c2) -> IfThenElse (subst_e e1, subst_c c1, subst_c c2)
    | Whiledo (e1, c1) -> Whiledo (subst_e e1, subst_c c1)
    | Dowhile (e1, c1) -> Dowhile (subst_e e1, subst_c c1)
    | For (c1, e1, c2, c3) ->
        For (subst_c c1, subst_e e1, subst_c c2, subst_c c3)
    | Expr e -> Expr (subst_e e)
    | Switch (e1, ecl) ->
        Switch (subst_e e1, List.map (fun (e, c) -> (subst_e e, subst_c c)) ecl)
    | Return eo -> Return (Option.map subst_e eo)
    | Match (e1, mel, t) -> Match (subst_e e1, mel, Option.map subst_maybe t))

let rec is_type_generic (t : perktype) : bool =
  match t with
  | _, Basetype _, _ -> Hashtbl.mem File_info.generic_types_table t
  | _, Funtype (tl, t1), _ ->
      List.exists is_type_generic tl || is_type_generic t1
  | _, Lambdatype (tl, t1, pvdl), _ ->
      List.exists is_type_generic tl
      || is_type_generic t1
      || List.exists (fun (t, _) -> is_type_generic t) pvdl
  | _, Pointertype t1, _ -> is_type_generic t1
  | _, Arraytype (t1, _), _ -> is_type_generic t1
  | _ ->
      (* TODO !!!! I REALLY REALLY REALLY REALLY HAVE NO WILL TO WRITE THIS FUNCTION 
         RIGHT NOW PLEASE GOD LET SOMEBODY ELSE WRITE THIS, FUCKING HELL*)
      false
