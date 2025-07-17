(** Defines exceptions for Perk lexing, parsing, type-checking and code
    generation. *)

open Ast

exception ParseError of string * string
exception Syntax_error of (int * int) * (int * int) * string * string
exception Lexing_error of (int * int) * (int * int) * string * string
exception Compilation_error of (int * int) * (int * int) * string * string
exception Type_error of (int * int) * (int * int) * string * string
exception Type_match_error of string
exception Double_declaration of string
exception Undeclared of string
exception Not_inferred of string

let raise_type_error (node : 'a annotated) (msg : string) =
  raise
    (Type_error
       ( (( @@ ) node).start_pos,
         (( @@ ) node).end_pos,
         (( @@ ) node).filename,
         msg ))

let raise_syntax_error (node : 'a annotated) (msg : string) =
  raise
    (Syntax_error
       ( (( @@ ) node).start_pos,
         (( @@ ) node).end_pos,
         (( @@ ) node).filename,
         msg ))

let raise_compilation_error (node : 'a annotated) (msg : string) =
  raise
    (Compilation_error
       ( (( @@ ) node).start_pos,
         (( @@ ) node).end_pos,
         (( @@ ) node).filename,
         msg ))
