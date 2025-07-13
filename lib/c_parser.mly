%{
(** Parser for C types. *)

  open C_errors
  open Ctype_ast
%}


/* Tokens declarations */
%token Const Restrict Volatile Long Short Signed Unsigned
%token EOF Comma Semicolon LParen RParen LBrace RBrace Star Div Ellipsis
%token TypeRefTypeName Signature
%token <int> Integer
%token <float> Float
%token <string> Ident
%token <string> BaseSort


/* Precedence and associativity specification */

/* Starting symbol */
%start <ctype> return_type
%start <typed_var list> signature

%%

/* Grammar specification */

return_type:
  | TypeRefTypeName c = ctype EOF {c}

signature:
  | Signature LParen RParen EOF {[]}
  | Signature LParen cl=arg_list RParen EOF {cl}

typed_var:
  | c = ctype d = declarator {TypedVar(c, d)}
  | Ellipsis {Ellipsis}

arg_list:
  | c = typed_var {[c]}
  | cl = arg_list Comma c = typed_var {cl @ [c]}

ctype:
  | s = specifier_list {CBaseType (s)}

  
specifier_list:
  | s = specifier {[s]}
  | s = specifier sl = specifier_list  {s :: sl}
  | specifier error {raise (ParseError "expected another specifier")}

specifier:
  | Long          {Long}
  | Short         {Short}
  | Signed        {Signed}
  | Unsigned      {Unsigned}
  | Const         {Qual Const}
  | Volatile      {Qual Volatile}
  | Restrict      {Qual Restrict}
  | s = BaseSort  {CBaseSort s}
  | s = stars     {PtrLevel s}

stars:
  | Star {1}
  | Star s = stars {s + 1}

declarator:
  | i = Ident {IdenDecl i}
%%
