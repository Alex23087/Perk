%{
  open Errors
  open Ast
  open Utils
  open Parse_lexing_commons
%}

/* Tokens declarations */
%token EOF
%token Plus Eq Neq Lt Leq Gt Geq Minus Star Div Ampersand PlusPlus MinusMinus Dot Ellipsis Question Land Lor ShL ShR
%token Fun Assign If Then Else While Do For
%token <bool> Boolean
%token <int> Integer
%token <float> Float
%token <char> Character
%token <string> String
%token <string> Ident
%token Comma Semicolon Colon LParen RParen LBrace RBrace LBracket RBracket Bang
%token Arrow Bigarrow As
%token Skip Return Let Continue Break
%token Public Private Static Extern
%token Const Volatile Restrict
%token <string> InlineC
%token Import ImportLocal Open
%token Archetype Model Summon Banish Cast
%token Struct Make
%token ADT Pipe Match When Matchall Constr Var BTICK
%token Nothing Something Of

/* Precedence and associativity specification */
%left Semicolon
%left Comma
%nonassoc Lt Leq Gt Geq
%nonassoc Eq
%left Arrow
%left Plus Minus
%left Star Div As
%right PlusPlus MinusMinus Bang Ampersand
                          /* The above line is intended for prefix operators.
                             (You would use %prec with these tokens in your grammar
                             so that, for example, a pointer dereference (prefix Star)
                             binds tighter than binary Star/Div.) */
%nonassoc POSTFIX         /* Intended for postfix operators (e.g. post ++/--)
                             via %prec POSTFIX in the corresponding rules */
%left Dot


/* Starting symbol */
%start program
%type <Ast.topleveldef_a list> program
%type <Ast.topleveldef_a> topleveldef
%type <Ast.command_a> command
%type <Ast.perkdef> perkdef
%type <Ast.deforfun_a> deforfun
%type <Ast.perkvardesc> perkvardesc
%type <Ast.perkfundef> perkfun
%type <Ast.perktype> perktype
%type <Ast.perktype_partial> perkfuntype
%type <Ast.binop> binop
%type <Ast.preunop> preunop
%type <Ast.postunop> postunop
%type <Ast.expr_a> expr
%type <Ast.perkident list> ident_list
%type <Ast.perktype list> perktype_list
%type <Ast.command_a> if_command
%type <perkident * (Ast.perktype list)> constructor_type

// %on_error_reduce command

%%


/* Grammar specification */

program:
  | defs = nonempty_list(topleveldef) EOF                                                                  { defs }
  | separated_list(Semicolon, topleveldef) error                                                           { raise (ParseError(!fnm, "unexpected token after program (Perhaps you forgot a ; ?)")) }
  | EOF                                                                                                    { raise (ParseError(!fnm, "empty program")) }

constructor_type:
  | i = Ident LParen l = separated_list(Comma, perktype) RParen                                            {add_constructor_name i; (i, l) }
  | i = Ident                                                                                              {add_constructor_name i ; (i, []) }  | i = Ident error                                                                                        { raise (ParseError(!fnm, ("invalid type for constructor" ^ i))) }
  | error                                                                                                  { raise (ParseError(!fnm, "constructor type expected")) }

topleveldef:
  | Import i = String                                                                                      { annotate_2_code !fnm $loc (Ast.Import ("<" ^ i ^ ">")) }
  | ImportLocal i = String                                                                                 { annotate_2_code !fnm $loc (Ast.Import ("\"" ^ i ^ "\"")) }
  | Open i = String                                                                                        { annotate_2_code !fnm $loc (Ast.Open i) }
  | Extern id = Ident Colon t = perktype                                                                   { annotate_2_code !fnm $loc (Ast.Extern (id, t)) }
  | ic = InlineC                                                                                           { annotate_2_code !fnm $loc (Ast.InlineC(ic)) }
  | d = perkdef                                                                                            { annotate_2_code !fnm $loc (Ast.Def (d, None)) }
  | Archetype i = Ident LBrace l = perkdeclorfun_list RBrace                                               { annotate_2_code !fnm $loc (Ast.Archetype (i, l)) }
  | Model i = Ident Colon il = ident_list LBrace l = perkdeforfun_list RBrace                              { annotate_2_code !fnm $loc (Ast.Model (i, il, l)) }
  | Model i = Ident LBrace l = perkdeforfun_list RBrace                                                    { annotate_2_code !fnm $loc (Ast.Model (i, [], l)) }
  | Struct i = Ident LBrace l = separated_list(Comma, perkdef) RBrace                                      { annotate_2_code !fnm $loc (Ast.Struct (i, l)) }
  | Struct Ident LBrace error                                                                              { raise (ParseError(!fnm, "unexpected token in struct definition")) }
  | ADT i = Ident Assign option(Pipe) l = separated_nonempty_list(Pipe, constructor_type)                  { annotate_2_code !fnm $loc (Ast.ADT (i, l)) }   
  | ADT Ident error                                                                                        { raise (ParseError(!fnm, "expected a list of constructors after ADT definition")) }           
  | Fun pf = perkfun                                                                                       { annotate_2_code !fnm $loc (Ast.Fundef (pf)) }
  | error                                                                                                  { raise (ParseError(!fnm, "top-level definition expected")) }


if_command:
  | If LParen e = expr RParen LBrace c1 = command RBrace Else LBrace c2 = command RBrace                   { annotate_2_code !fnm $loc (Ast.IfThenElse (e, c1, c2)) }
  | If LParen e = expr RParen LBrace c1 = command RBrace Else c2 = if_command                              { annotate_2_code !fnm $loc (Ast.IfThenElse (e, c1, c2)) }
  | If LParen e = expr RParen LBrace c1 = command RBrace                                                   { annotate_2_code !fnm $loc (Ast.IfThenElse (e, c1, annotate_dummy Ast.Skip)) }

command:
  | ic = InlineC                                                                                           { annotate_2_code !fnm $loc (Ast.InlineCCmd(ic)) }
  | d = perkdef                                                                                            { annotate_2_code !fnm $loc (Ast.DefCmd (d, None)) }
  | l = expr Assign r = expr                                                                               { annotate_2_code !fnm $loc (Ast.Assign (l, r, None, None)) }
  | if_command                                                                                             { $1 }
  | While LParen e = expr RParen LBrace c = command RBrace                                                 { annotate_2_code !fnm $loc (Ast.Whiledo (e, c)) }
  | Do LBrace c = command RBrace While LParen e = expr RParen                                              { annotate_2_code !fnm $loc (Ast.Dowhile (e, c)) }
  | For LParen c1 = command Semicolon e2 = expr Semicolon c3 = command RParen LBrace body = command RBrace { annotate_2_code !fnm $loc (Ast.For (c1, e2, c3, body)) }
  | LBrace c = command RBrace                                                                              { annotate_2_code !fnm $loc (Ast.Block(c)) }
  | e = expr                                                                                               { annotate_2_code !fnm $loc (Ast.Expr(e)) }
  | c1 = command Semicolon c2 = command                                                                    { annotate_2_code !fnm $loc (Ast.Seq (c1, c2)) }
  | c1 = command Semicolon                                                                                 { c1 }
  | Skip                                                                                                   { annotate_2_code !fnm $loc (Ast.Skip) }
  | Return                                                                                                 { annotate_2_code !fnm $loc (Ast.Return (None)) }
  | Return e = expr                                                                                        { annotate_2_code !fnm $loc (Ast.Return (Some e)) }
  | Banish i = Ident                                                                                       { annotate_2_code !fnm $loc (Banish i) }
  | Continue                                                                                               { annotate_2_code !fnm $loc (Ast.Continue) }
  | Break                                                                                                  { annotate_2_code !fnm $loc (Ast.Break) }
  | Match LParen e = expr RParen LBrace l = separated_nonempty_list (Comma, match_entry) RBrace            {annotate_2_code !fnm $loc (Ast.Match(e, l, None))}
  | Match LParen expr RParen LBrace separated_list (Comma, match_entry) error                              { raise (ParseError(!fnm, "invalid match statement (perhaps you are missing a ',' between cases?)")) }
  | Match error                                                                                            { raise (ParseError(!fnm, "invalid match scrutinee (perhaps you are missing a '(' ?)")) }
  | error                                                                                                  { raise (ParseError(!fnm, "command expected")) }
  | command error                                                                                          { raise (ParseError(!fnm, "unexpected command (perhaps you are missing a ';'?)")) }
  | expr Assign error                                                                                      { raise (ParseError(!fnm, "expression expected on the right hand side of =")) }
  | For LParen command Semicolon expr Semicolon command RParen error                                       { raise (ParseError(!fnm, "missing braces after for guard"))}
  | If LParen expr RParen LBrace command RBrace Else error                                                 { raise (ParseError(!fnm, "missing braces after else"))}
  | If LParen expr RParen error                                                                            { raise (ParseError(!fnm, "missing braces after if guard"))}
  | While LParen expr RParen error                                                                         { raise (ParseError(!fnm, "missing braces after while guard"))}
  | Do error                                                                                               { raise (ParseError(!fnm, "missing braces after do"))}

match_entry:
  | m = match_case LBrace c = command RBrace                                                               {annotate_2_code !fnm $loc (Ast.MatchCase(m, None, c))}
  | m = match_case When e = expr LBrace c = command RBrace                                                 {annotate_2_code !fnm $loc (Ast.MatchCase(m, Some e, c))}
  | match_case error                                                                                       { raise (ParseError(!fnm, "invalid match case, expected case body")) }
  | error                                                                                                  { raise (ParseError(!fnm, "match case expected")) }

match_case:
  | BTICK LBrace e = expr RBrace {annotate_2_code !fnm $loc (Ast.MatchExpr(e))}
  | Matchall {annotate_2_code !fnm $loc Ast.Matchall}
  | Var i=Ident {annotate_2_code !fnm $loc (Ast.MatchVar(i, None))}
  | Var i=Ident Colon t=perktype {annotate_2_code !fnm $loc (Ast.MatchVar(i, Some t))}
  | i=Ident { annotate_2_code !fnm $loc (Ast.CompoundCase(i, []))}
  | i=Ident LParen l = separated_nonempty_list(Comma, match_case) RParen { annotate_2_code !fnm $loc (Ast.CompoundCase(i, l))}
  | error { raise (ParseError(!fnm, "expected match case")) }

deforfun:
  | d = perkdef                                                                                            {annotate_2_code !fnm $loc (Ast.DefVar([], d))}
  | Fun d = perkfun                                                                                        {annotate_2_code !fnm $loc (Ast.DefFun([], d))}

perkdef:
  | Let vd = perkvardesc Assign e = expr                                                                   { (vd, e) }
  | Let perkvardesc error                                                                                  { raise (ParseError(!fnm, "expression expected: value must be initialized")) }
  | error { raise (ParseError(!fnm, "definition expected (e.g. let banana : int = 5)")) }

perkfun:
  | i = Ident LParen id_list = perkvardesc_list RParen Colon rt = perktype LBrace c = command RBrace       { (rt, i, id_list, c) }
  | i = Ident LParen RParen Colon rt = perktype LBrace c = command RBrace                                  { (rt, i, [], c) }
  | Ident LParen perkvardesc_list RParen error                                                             { raise (ParseError(!fnm, "invalid function definition (Did you forget to specify the return type?)")) }
  | Ident LParen RParen error                                                                              { raise (ParseError(!fnm, "invalid function definition (Did you forget to specify the return type?)")) }
  

perkvardesc:
  | i = Ident Colon t = perktype                                                                           { (t, i) }
  | i = Ident Colon                                                                                        { (([], Ast.Infer, []), i) }
  | error { raise (ParseError(!fnm, "variable descriptor expected (e.g. banana : int)")) }
  | Ident error { raise (ParseError(!fnm, "variable descriptor expected (e.g. banana : int)")) }

perkfundesc:
  | Fun i = Ident Colon t = perktype                                                                       { (t, i) }

declorfun:
  | d = perkvardesc                                                                                        { annotate_2_code !fnm $loc (Ast.DeclVar d) }
  | f = perkfundesc                                                                                        { annotate_2_code !fnm $loc (Ast.DeclFun f) }

expr:
  | e1 = expr LParen args = separated_list(Comma, expr) RParen                                             { annotate_2_code !fnm $loc (Ast.Apply (e1, args, None)) }
  | e1 = expr b = binop e2 = expr                                                                          { annotate_2_code !fnm $loc (Ast.Binop (b, e1, e2)) }
  | u = preunop e = expr                                                                                   { annotate_2_code !fnm $loc (Ast.PreUnop (u, e)) }
  | e = expr u = postunop %prec POSTFIX                                                                    { annotate_2_code !fnm $loc (Ast.PostUnop (u, e)) }
  | LParen id_list = perkvardesc_list RParen Colon ret = perktype LBrace c = command RBrace                { annotate_2_code !fnm $loc (Ast.Lambda (ret, id_list, c, [], None)) }
  | LParen RParen Colon ret = perktype LBrace c = command RBrace                                           { annotate_2_code !fnm $loc (Ast.Lambda (ret, [], c, [], None)) }
  | b = Boolean                                                                                            { annotate_2_code !fnm $loc (Ast.Bool (b)) }
  | n = Integer                                                                                            { annotate_2_code !fnm $loc (Ast.Int (n)) }
  | f = Float                                                                                              { annotate_2_code !fnm $loc (Ast.Float (f)) }
  | c = Character                                                                                          { annotate_2_code !fnm $loc (Ast.Char (c)) }
  | s = String                                                                                             { annotate_2_code !fnm $loc (Ast.String (s)) }
  | i = Ident                                                                                              { annotate_2_code !fnm $loc (Ast.Var(i)) }
  | LParen e = expr RParen                                                                                 { annotate_2_code !fnm $loc (Ast.Parenthesised e) }
  | e1 = expr LBracket e2 = expr RBracket                                                                  { annotate_2_code !fnm $loc (Ast.Subscript (e1, e2)) }
  | Summon i = Ident LParen l = expr_list RParen                                                           { annotate_2_code !fnm $loc (Summon (i, l)) }
  | Summon i = Ident LParen RParen                                                                         { annotate_2_code !fnm $loc (Summon (i, [])) }
  | Make i = Ident LParen RParen                                                                           { annotate_2_code !fnm $loc (Ast.Make (i, [])) }
  | Make i = Ident LParen l = initializer_list RParen                                                      { annotate_2_code !fnm $loc (Ast.Make (i, l)) }
  | Make Ident LParen error                                                                                { raise (ParseError(!fnm, "invalid make expression (perhaps you forgot a closing parenthesis?)")) }
  | e1 = expr Dot i = Ident                                                                                { annotate_2_code !fnm $loc (Ast.Access (e1, i, None, None)) }
  | Nothing                                                                                                { annotate_2_code !fnm $loc (Ast.Nothing ([], Ast.Infer, [])) }
  | Nothing Of t=perktype                                                                                  { annotate_2_code !fnm $loc (Ast.Nothing (t)) }
  | Something e = expr                                                                                     { annotate_2_code !fnm $loc (Ast.Something (e, ([], Ast.Infer, []))) }
  | LParen RParen                                                                                          { annotate_2_code !fnm $loc (Ast.Tuple ([], None)) }
  | LParen e = expr_list RParen                                                                            { annotate_2_code !fnm $loc (Ast.Tuple (e, None)) }
  | e = expr As tl = separated_nonempty_list (Plus, perktype)                                              { annotate_2_code !fnm $loc (Ast.As (e, tl, None)) }
  | LBracket RBracket                                                                                      { annotate_2_code !fnm $loc (Ast.Array [])}
  | LBracket l = separated_nonempty_list (Comma, expr) RBracket                                            { annotate_2_code !fnm $loc (Ast.Array l)}
  | Cast LParen typ = perktype Comma e = expr RParen                                                       { annotate_2_code !fnm $loc (Ast.Cast ((([],Ast.Infer,[]), typ), e)) }
  | If guard = expr Then e1 = expr Else e2 = expr                                                          { annotate_2_code !fnm $loc (Ast.IfThenElseExpr (guard, e1, e2)) }

  | error                                                                                                  { raise (ParseError(!fnm, "expression expected")) }
  | expr error                                                                                             { raise (ParseError(!fnm, "unexpected expression")) }
  | Ident error                                                                                            { raise (ParseError(!fnm, "unexpected expression. Perhaps you tried to use C-style types?")) }
  | Summon Ident error                                                                                     { raise (ParseError(!fnm, "error while summoning")) }
  | LParen perkvardesc_list RParen Colon perktype error                                                    { raise (ParseError(!fnm, "invalid lambda definition (Perhaps you are missing a => )")) }
  | LParen RParen Colon perktype error                                                                     { raise (ParseError(!fnm, "invalid lambda definition (Perhaps you are missing a => )")) }

%inline perktype_attribute:
  | Public                                                                                                 { Ast.Public }
  | Private                                                                                                { Ast.Private }
  | Static                                                                                                 { Ast.Static }

%inline perktype_qualifier:
  | Const                                                                                                  { Ast.Const }
  | Volatile                                                                                               { Ast.Volatile }
  | Restrict                                                                                               { Ast.Restrict }

%inline perkfuntype:
  | t1 = perktype Arrow t2 = perktype                                                                      { Ast.Funtype ([t1], t2) }
  | LParen RParen Arrow t = perktype                                                                       { Ast.Funtype ([], t) }
  | LParen tl = perktype_list RParen Arrow tf = perktype                                                   { Ast.Funtype (tl, tf) }

%inline perklamtype:
  | t1 = perktype Bigarrow t2 = perktype                                                                   { Ast.Lambdatype ([t1], t2, []) }
  | LParen RParen Bigarrow t = perktype                                                                    { Ast.Lambdatype ([], t, []) }
  | LParen tl = perktype_list RParen Bigarrow tf = perktype                                                { Ast.Lambdatype (tl, tf, []) }

perktype:
  | t = perktype_partial q = list(perktype_qualifier)                                                      { ([], t, q) }
  | a = nonempty_list(perktype_attribute) t = perktype_partial q = list(perktype_qualifier)                { (a, t, q) }
  | t = perkfuntype q = list(perktype_qualifier)                                                           { ([], t, q) }
  | a = nonempty_list(perktype_attribute) t = perkfuntype q = list(perktype_qualifier)                     { (a, t, q) }
  | t = perklamtype q = list(perktype_qualifier)                                                           { ([], t, q) }
  | a = nonempty_list(perktype_attribute) t = perklamtype q = list(perktype_qualifier)                     { (a, t, q) }
  | LParen t = perktype RParen                                                                             { t }
  | Ellipsis                                                                                               { ([], Ast.Vararg, []) }
  | error                                                                                                  { raise (ParseError(!fnm, "type expected")) }

perktype_partial:
  | i = Ident                                                                                              { Ast.Basetype i }
  | LBracket t = perktype RBracket                                                                         { Ast.Arraytype (t, None) }
  | LBracket t = perktype n = Integer RBracket                                                             { Ast.Arraytype (t, Some n) }
  | LParen tl = separated_nonempty_list (Star, perktype) RParen                                            { Ast.Tupletype(tl)}
  | LParen RParen                                                                                          { Ast.Tupletype [] }
  | t = perktype Star                                                                                      { Ast.Pointertype t }
  | t = perktype Question                                                                                  { Ast.Optiontype t }
  | Lt tys = separated_nonempty_list(Plus, Ident) Gt                                                       { Ast.ArchetypeSum (tys |> List.map (fun x -> ([], Ast.Basetype x, []))) }
  | error                                                                                                  { raise (ParseError(!fnm, "type expected")) }
  | Lt error                                                                                               { raise (ParseError(!fnm, "Cannot have empty archetype sum")) }
  | Lt separated_nonempty_list(Plus, Ident) error                                                          { raise (ParseError(!fnm, "Unterminated archetype sum")) }

%inline binop:
  | Plus                                                                                                   { Ast.Add }
  | Minus                                                                                                  { Ast.Sub }
  | Star                                                                                                   { Ast.Mul }
  | Div                                                                                                    { Ast.Div }
  | Eq                                                                                                     { Ast.Eq }
  | Lt                                                                                                     { Ast.Lt }
  | Leq                                                                                                    { Ast.Leq }
  | Gt                                                                                                     { Ast.Gt }
  | Geq                                                                                                    { Ast.Geq }
  | Neq                                                                                                    { Ast.Neq }
  | Land                                                                                                   { Ast.Land }
  | Lor                                                                                                    { Ast.Lor }
  | ShL                                                                                                    { Ast.ShL }
  | ShR                                                                                                    { Ast.ShR }

%inline preunop:
  | Minus                                                                                                  { Ast.Neg }
  | Bang                                                                                                   { Ast.Not }
  | Ampersand                                                                                              { Ast.Reference }
  | Star                                                                                                   { Ast.Dereference }
  | PlusPlus                                                                                               { Ast.PreIncrement }
  | MinusMinus                                                                                             { Ast.PreDecrement }

%inline postunop:
  | PlusPlus                                                                                               { Ast.PostIncrement }
  | MinusMinus                                                                                             { Ast.PostDecrement }
  | Bang                                                                                                   { Ast.OptionGet None }
  | Question                                                                                               { Ast.OptionIsSome }


/* New nonterminals for disambiguated lists */

expr_list:
  | e = expr { [e] }
  | el = expr_list Comma e = expr { el @ [e] }
  | error { raise (ParseError(!fnm, "expression expected")) }

ident_list:
  | i = Ident { [i] }
  | il = ident_list Comma i = Ident { il @ [i] }
  | error { raise (ParseError(!fnm, "identifier expected")) }
  | Ident error { raise (ParseError(!fnm, "unexpected identifier")) }

perktype_list:
  | t = perktype { [t] }
  | tl = perktype Comma t = perktype_list { tl :: t }
  | error { raise (ParseError(!fnm, "type expected")) }
  | perktype error { raise (ParseError(!fnm, "unexpected type")) }

perkdeforfun_list:
  | t = deforfun { [t] }
  | a = nonempty_list(perktype_attribute) t = deforfun { [add_attrs_to_deforfun a t] }
  | tl = deforfun Comma t = perkdeforfun_list { tl :: t }
  | a = nonempty_list(perktype_attribute) tl = deforfun Comma t = perkdeforfun_list { (add_attrs_to_deforfun a tl) :: t }
  | error { raise (ParseError(!fnm, "definition expected")) }
  | deforfun error { raise (ParseError(!fnm, "unexpected definition")) }

perkvardesc_list:
  | t = perkvardesc { [t] }
  | tl = perkvardesc Comma t = perkvardesc_list { tl :: t }
  | error { raise (ParseError(!fnm, "variable descriptor expected")) }
  | perkvardesc error { raise (ParseError(!fnm, "unexpected variable descriptor")) }

perkdeclorfun_list:
  | t = declorfun { [t] }
  | tl = declorfun Comma t = perkdeclorfun_list { tl :: t }
  | error { raise (ParseError(!fnm, "variable descriptor expected")) }
  | declorfun error { raise (ParseError(!fnm, "unexpected variable descriptor")) }

initializer_list:
  | i = Ident Assign e = expr { [(i, e)] }
  | i = Ident Assign e = expr Comma il = initializer_list { (i, e) :: il }
  | Ident error { raise (ParseError(!fnm, "initializer expected (e.g. field = value)")) }
  | error { raise (ParseError(!fnm, "initializer expected (e.g. field = value)")) }

spanish_inquisition:
  | error { raise (ParseError(!fnm, "Nobody expects the Spanish Inquisition!")) }
%%