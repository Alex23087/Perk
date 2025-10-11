(** Lexer for C types. *)

open C_parser

exception Lexing_error of (int * int) * (int * int) * string

let string_buffer = Buffer.create 512
let digit = [%sedlex.regexp? '0' .. '9']
let hex_digit = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f' | 'A' .. 'F']
let hex_number = [%sedlex.regexp? Plus hex_digit]
let dec_number = [%sedlex.regexp? Plus digit]
let oct_number = [%sedlex.regexp? Plus '0' .. '7']

let float_number =
  [%sedlex.regexp? Plus digit, '.', Star digit | Star digit, '.', Plus digit]
(* Allow for numbers all of the following formats: 0.5, .5, 1. *)

let character = [%sedlex.regexp? 0x20 .. 0x7E]

let identifier =
  [%sedlex.regexp?
    ('a' .. 'z' | 'A' .. 'Z' | '_'), Star ('a' .. 'z' | 'A' .. 'Z' | digit | '_')]

let white_space = [%sedlex.regexp? ' ' | '\t' | '\n' | '\r']

let escape =
  [%sedlex.regexp?
    '\\', ('0' | '\'' | '"' | 'b' | 'f' | 't' | '\\' | 'r' | 'n')]

let stringchar = [%sedlex.regexp? 0x00 .. 0xFF]

let unescape ch lexbuf =
  match ch with
  | '0' -> char_of_int 0x00
  | '\'' -> '\''
  | '"' -> '"'
  | 'b' -> '\b'
  | 'f' -> char_of_int 0x0C
  | 't' -> '\t'
  | '\\' -> '\\'
  | 'r' -> '\r'
  | 'n' -> '\n'
  | _ ->
      let start_pos =
        ( (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum,
          (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
          - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol )
      in
      let end_pos =
        ( (snd (Sedlexing.lexing_positions lexbuf)).pos_lnum,
          (snd (Sedlexing.lexing_positions lexbuf)).pos_cnum
          - (snd (Sedlexing.lexing_positions lexbuf)).pos_bol )
      in
      raise (Lexing_error (start_pos, end_pos, "Invalid escape character"))

let type_names = ref [ "int"; "char"; "float"; "void"; "bool" ]

let rec token lexbuf =
  match%sedlex lexbuf with
  | "..." -> Ellipsis
  | "typeref:typename:" -> TypeRefTypeName
  | "signature:" -> Signature
  | "const" -> Const
  | "__restrict" -> Restrict
  | "volatile" -> Volatile
  | "long" -> Long
  | "short" -> Short
  | "signed" -> Signed
  | "unsigned" -> Unsigned
  | identifier ->
      let s = Sedlexing.Latin1.lexeme lexbuf in
      if List.mem s !type_names then BaseSort s else Ident s
  | "0x", hex_number -> Integer (int_of_string (Sedlexing.Latin1.lexeme lexbuf))
  | "0b", Plus ('0' | '1') ->
      Integer (int_of_string (Sedlexing.Latin1.lexeme lexbuf))
  | "0o", oct_number ->
      Integer
        (int_of_string
           ("0o"
           ^
           let s = Sedlexing.Latin1.lexeme lexbuf in
           String.sub s 2 (String.length s - 2)))
  | dec_number -> Integer (int_of_string (Sedlexing.Latin1.lexeme lexbuf))
  | white_space -> token lexbuf
  | "," -> Comma
  | ";" -> Semicolon
  | "(" -> LParen
  | ")" -> RParen
  | "{" -> LBrace
  | "}" -> RBrace
  | "*" -> Star
  | "//" -> comment lexbuf
  | "/*" -> multiline_comment lexbuf
  | eof -> EOF
  (* | inline_c_content, "}" -> INLINEC_CONTENT (Sedlexing.Latin1.lexeme lexbuf) *)
  | any ->
      let start_pos =
        ( (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum,
          (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
          - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol )
      in
      let end_pos =
        ( (snd (Sedlexing.lexing_positions lexbuf)).pos_lnum,
          (snd (Sedlexing.lexing_positions lexbuf)).pos_cnum
          - (snd (Sedlexing.lexing_positions lexbuf)).pos_bol )
      in
      raise
        (Lexing_error
           ( start_pos,
             end_pos,
             Printf.sprintf "Unrecognised character: '%s'"
               (Sedlexing.Latin1.lexeme lexbuf) ))
  | _ -> failwith "Impossible!"

and comment lexbuf =
  match%sedlex lexbuf with
  | "\n" -> token lexbuf
  | eof -> EOF
  | any -> comment lexbuf
  | _ -> failwith "Impossible!"

and multiline_comment lexbuf =
  match%sedlex lexbuf with
  | "*/" -> token lexbuf
  | eof -> EOF
  | any -> multiline_comment lexbuf
  | _ -> failwith "Impossible!"

and inlineC lexbuf =
  match%sedlex lexbuf with
  | "END_C" -> ()
  | any ->
      Buffer.add_string string_buffer (Sedlexing.Latin1.lexeme lexbuf);
      inlineC lexbuf
  | _ ->
      let start_pos =
        ( (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum,
          (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
          - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol )
      in
      let end_pos =
        ( (snd (Sedlexing.lexing_positions lexbuf)).pos_lnum,
          (snd (Sedlexing.lexing_positions lexbuf)).pos_cnum
          - (snd (Sedlexing.lexing_positions lexbuf)).pos_bol )
      in
      raise (Lexing_error (start_pos, end_pos, "Inline C not closed by END_C!"))

and string_literal lexbuf =
  match%sedlex lexbuf with
  | '"' -> ()
  | eof ->
      let start_pos =
        ( (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum,
          (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
          - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol )
      in
      let end_pos =
        ( (snd (Sedlexing.lexing_positions lexbuf)).pos_lnum,
          (snd (Sedlexing.lexing_positions lexbuf)).pos_cnum
          - (snd (Sedlexing.lexing_positions lexbuf)).pos_bol )
      in
      raise (Lexing_error (start_pos, end_pos, "Unterminated string"))
  | escape ->
      let chara = Sedlexing.Latin1.lexeme lexbuf in
      Buffer.add_char string_buffer
        (match chara.[0] with '\\' -> unescape chara.[1] lexbuf | c -> c);
      string_literal lexbuf
  | stringchar ->
      Buffer.add_string string_buffer (Sedlexing.Latin1.lexeme lexbuf);
      string_literal lexbuf
  | _ ->
      (* Handle invalid characters in string literal *)
      let start_pos =
        ( (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum,
          (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
          - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol )
      in
      let end_pos =
        ( (snd (Sedlexing.lexing_positions lexbuf)).pos_lnum,
          (snd (Sedlexing.lexing_positions lexbuf)).pos_cnum
          - (snd (Sedlexing.lexing_positions lexbuf)).pos_bol )
      in
      raise
        (Lexing_error (start_pos, end_pos, "Invalid character in string literal"))

let tokenize (lexbuf : Sedlexing.lexbuf) = token lexbuf
