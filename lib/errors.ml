(** Defines exceptions for Perk lexing, parsing, type-checking and code
    generation. *)

open Ast
open Error_codes

exception ParseError of string * string * error_code

exception
  Syntax_error of (int * int) * (int * int) * string * string * error_code

exception
  Lexing_error of (int * int) * (int * int) * string * string * error_code

exception
  Compilation_error of (int * int) * (int * int) * string * string * error_code

exception Type_error of (int * int) * (int * int) * string * string * error_code
exception Type_match_error of string
exception Double_declaration of string
exception Undeclared of string
exception Not_inferred of string

let raise_type_error (node : 'a annotated) (msg : string) (code : error_code) =
  raise
    (Type_error
       ( (( @@ ) node).start_pos,
         (( @@ ) node).end_pos,
         (( @@ ) node).filename,
         msg,
         code ))

let raise_syntax_error (node : 'a annotated) (msg : string) (code : error_code)
    =
  raise
    (Syntax_error
       ( (( @@ ) node).start_pos,
         (( @@ ) node).end_pos,
         (( @@ ) node).filename,
         msg,
         code ))

let raise_compilation_error (node : 'a annotated) (msg : string)
    (code : error_code) =
  raise
    (Compilation_error
       ( (( @@ ) node).start_pos,
         (( @@ ) node).end_pos,
         (( @@ ) node).filename,
         msg,
         code ))
