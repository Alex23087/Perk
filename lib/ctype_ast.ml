(** Abstract Syntax Tree of C types. *)

type qualifier =
  | Const
  | Volatile
  | Restrict
[@@deriving show]

type specifier =
  | Long
  | Short
  | Signed
  | Unsigned
  | Qual of qualifier
  | CBaseSort of string
  | PtrLevel of int
[@@deriving show]

type declarator = IdenDecl of string [@@deriving show]
type ctype = CBaseType of specifier list [@@deriving show]

type typed_var =
  | TypedVar of ctype * declarator
  | Ellipsis
[@@deriving show]
