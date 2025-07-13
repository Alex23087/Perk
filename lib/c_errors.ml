(** Defines exceptions that can be called when C parsing fails. *)

exception ParseError of string
exception Syntax_error of (int * int) * (int * int) * string
