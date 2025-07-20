(** Abstract Syntax Tree of a Perk program. *)

type 'a annotated = {
  loc : Location.location; [@opaque]
  node : 'a;
}
[@@deriving show, eq]

let ( @@ ) (annotated_node : 'a annotated) : Location.location =
  annotated_node.loc

let ( $ ) (annotated_node : 'a annotated) : 'a = annotated_node.node

let annotate (node : 'a) (loc : Location.location) : 'a annotated =
  { loc; node }

let annotate_2 (loc : Location.location) (node : 'a) = { loc; node }

let annotate_code (node : 'a) (start_pos, end_pos) filename : 'a annotated =
  let loc = Location.to_code_position (start_pos, end_pos, filename) in
  { loc; node }

let annotate_2_code filename (start_pos, end_pos) (node : 'a) : 'a annotated =
  let loc = Location.to_code_position (start_pos, end_pos, filename) in
  { loc; node }

let annotate_dummy (node : 'a) : 'a annotated =
  { loc = Location.dummy_pos; node }

let annot_copy (annotated_node : 'a annotated) (node : 'b) : 'b annotated =
  { loc = annotated_node.loc; node }

type perkident = string [@@deriving show, eq]

type perktype_attribute =
  | Public
  | Private
  | Static
[@@deriving show, eq]

type perktype_qualifier =
  | Const
  | Volatile
  | Restrict
[@@deriving show, eq]

(* type of the perk -- giangpt *)
type perktype_partial =
  | Basetype of string
  | Funtype of perktype list * perktype  (** types of args, return type *)
  | Lambdatype of
      perktype list
      * perktype
      * perkvardesc list (* last member is free var list *)
  | Pointertype of perktype
  | Arraytype of perktype * int option
  | Structtype of string * perkdef list
  | ArcheType of perkident * perkdecl list
  | Modeltype of
      perkident
      * perkident list
      * (perktype_attribute list * perkdecl) list
      * perktype list
      * perkident list
    (* name, archetypes, fields (with access attributes), constructor_params, member functions*)
  | Optiontype of perktype
  | Tupletype of perktype list
  | ArchetypeSum of perktype list
  | Vararg
  | Infer
[@@deriving show, eq]

and perktype =
  perktype_attribute list * perktype_partial * perktype_qualifier list
[@@deriving show, eq]

(* and perktype_annotated = perktype annotated [@@deriving show, eq] *)
and perkvardesc = perktype * perkident [@@deriving show, eq]
and perkdecl = perkvardesc [@@deriving show, eq]

and binop =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Lt
  | Leq
  | Gt
  | Geq
  | Neq
  | Land
  | Lor
(*  ... boolean and bitwise ops and all that  *)
[@@deriving show, eq]

and preunop =
  | Neg
  | Not
  | Dereference
  | Reference
  | PreIncrement
  | PreDecrement
[@@deriving show, eq]

and postunop =
  | PostIncrement
  | PostDecrement
  | OptionGet of perktype option
  | OptionIsSome
[@@deriving show, eq]

and perkdef = perkdecl * expr_a [@@deriving show, eq]

and perkfundef =
  perktype
  * perkident
  * perkvardesc list
  * command_a (* return, name, args, body *)

(* name, attributes, methods *)
(* and perklass = Class of perkident * (perkdef list) * (perkfun list) [@@deriving show, eq] *)
and expr_t =
  | Nothing of perktype
  | Something of expr_a * perktype
  | Bool of bool
  | Int of int
  | Float of float
  | Char of char
  | String of string
  | Var of perkident
  | Apply of expr_a * expr_a list * perktype option
  | Binop of binop * expr_a * expr_a
  | PreUnop of preunop * expr_a
  | Lambda of perktype * perkvardesc list * command_a * perkvardesc list
  | PostUnop of postunop * expr_a
  | Parenthesised of expr_a
  | Subscript of expr_a * expr_a
  | TupleSubscript of expr_a * int
  | Summon of perkident * expr_a list
  | Make of perkident * (perkident * expr_a) list
  | Access of expr_a * perkident * perktype option * perktype option
  | Tuple of expr_a list * perktype option
  | As of perkident * perktype list * perktype option
  | Array of expr_a list  (** Cast ((from_type, to_type), expression)*)
  | Cast of (perktype * perktype) * expr_a
  | IfThenElseExpr of expr_a * expr_a * expr_a
[@@deriving show, eq]

(* Syntax of the language *)
and command_t =
  | InlineCCmd of string
  | DefCmd of perkdef * perktype option
  | Block of command_a
  | Assign of (expr_a * expr_a * perktype option * perktype option)
  | Seq of command_a * command_a
  | IfThenElse of expr_a * command_a * command_a
  | Whiledo of expr_a * command_a
  | Dowhile of expr_a * command_a
  | For of command_a * expr_a * command_a * command_a
  | Expr of expr_a
  | Switch of expr_a * (expr_a * command_a) list
  | Skip
  | Banish of perkident
  | Return of expr_a option
  | Break
  | Continue
[@@deriving show, eq]

and topleveldef_t =
  | InlineC of string
  | Import of string
  | Open of string
  | Extern of perkident * perktype
  | Def of perkdef * perktype option
  | Fundef of perkfundef
  | Archetype of perkident * declorfun_a list
  | Model of perkident * perkident list * deforfun_a list
  | Struct of perkident * perkdef list

and deforfun_t =
  | DefVar of perktype_attribute list * perkdef
  | DefFun of perktype_attribute list * perkfundef

and declorfun_t =
  | DeclVar of perkdecl
  | DeclFun of perkdecl

and expr_a = expr_t annotated [@@deriving show, eq]
and command_a = command_t annotated [@@deriving show, eq]
and topleveldef_a = topleveldef_t annotated [@@deriving show, eq]
and deforfun_a = deforfun_t annotated [@@deriving show, eq]
and declorfun_a = declorfun_t annotated [@@deriving show, eq]

let all_vars : (perkident * perktype) list ref = ref []

let filter_var_table () =
  all_vars :=
    List.fold_left
      (fun l (id, typ) -> if List.mem (id, typ) l then l else (id, typ) :: l)
      [] !all_vars
