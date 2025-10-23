open Ast

(** maps polyfun ids to their signature and type parameter*)
let defined_polyfuns : (string, perktype list * perktype * perktype) Hashtbl.t =
  Hashtbl.create 10

(** maps polyfun ids to their instances*)
let polyfun_instances : (string, perktype list) Hashtbl.t = Hashtbl.create 10

let subst_perkvardesc ((pt, piden) : perkvardesc) (placeholder : perktype)
    (actual : perktype) =
  let subst_maybe t = if t = placeholder then actual else t in
  (subst_maybe pt, piden)

let rec subst_perkdef (pvd, e) (placeholder : perktype) (actual : perktype) :
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
  let subst_maybe t = if t = placeholder then actual else t in
  annot_copy mc
    (match ( $ ) mc with
    | Matchall -> Matchall
    | MatchVar (id, t) -> MatchVar (id, Option.map subst_maybe t)
    | MatchExpr e -> MatchExpr (subst_e e)
    | CompoundCase (id, mc1) ->
        CompoundCase (id, List.map (fun x -> subst_mc x placeholder actual) mc1))

and subst_type_expr (e : expr_a) (placeholder : perktype) (actual : perktype) =
  let subst_e = fun x -> subst_type_expr x placeholder actual in
  let subst_maybe t = if t = placeholder then actual else t in
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
    | PreUnop (op, e1) -> PreUnop (op, subst_e e1)
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
    | IfThenElseExpr (e1, e2, e3) ->
        IfThenElseExpr (subst_e e1, subst_e e2, subst_e e3))

and subst_type_command (c : command_a) (placeholder : perktype)
    (actual : perktype) =
  let subst_e = fun x -> subst_type_expr x placeholder actual in
  let subst_c = fun x -> subst_type_command x placeholder actual in
  let subst_maybe t = if t = placeholder then actual else t in
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

let subst_type t placeholder actual = if t = placeholder then actual else t
