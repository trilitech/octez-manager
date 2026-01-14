# 7 "src/ppx/exclude_lexer.mll"
 

type error =
  | Invalid_character of char
  | Unexpected_end_of_file

let string_of_error = function
  | Invalid_character ch -> Printf.sprintf "invalid character %C" ch
  | Unexpected_end_of_file -> "unexpected end of file"

let fail lexbuf error =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  raise (Exclude.Exception (pos.pos_lnum, string_of_error error))

let incr_line lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_lnum = succ pos.pos_lnum;
                         pos_bol = pos.pos_cnum }

let add_char prefix buf str =
  Buffer.add_char buf (Char.chr (int_of_string (prefix ^ str)))

let add_octal_char = add_char "0o"

let add_hexa_char = add_char "0x"


# 32 "src/ppx/exclude_lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\244\255\245\255\001\000\001\000\003\000\000\000\249\255\
    \000\000\000\000\000\000\253\255\254\255\255\255\000\000\002\000\
    \252\255\000\000\003\000\251\255\003\000\006\000\000\000\000\000\
    \250\255\248\255\246\255\002\000\245\255\246\255\019\000\092\000\
    \027\000\249\255\250\255\251\255\252\255\253\255\254\255\255\255\
    \035\000\248\255\115\000\247\255\005\000\250\255\251\255\006\000\
    \004\000\253\255\000\000\001\000\255\255\254\255\252\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\009\000\009\000\008\000\011\000\255\255\
    \011\000\011\000\011\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\010\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\003\000\
    \003\000\255\255\005\000\005\000\255\255\255\255\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\000\000\000\000\000\000\255\255\255\255\
    \000\000\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\028\000\000\000\000\000\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\000\000\255\255\000\000\045\000\000\000\000\000\255\255\
    \255\255\000\000\255\255\255\255\000\000\000\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\005\000\004\000\026\000\005\000\003\000\026\000\048\000\
    \054\000\054\000\047\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \005\000\000\000\007\000\005\000\029\000\000\000\000\000\049\000\
    \006\000\053\000\025\000\052\000\000\000\051\000\000\000\050\000\
    \000\000\000\000\000\000\000\000\000\000\034\000\000\000\000\000\
    \000\000\000\000\035\000\011\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\012\000\000\000\013\000\030\000\000\000\
    \000\000\017\000\000\000\000\000\000\000\020\000\010\000\016\000\
    \019\000\014\000\021\000\022\000\015\000\018\000\009\000\033\000\
    \024\000\000\000\008\000\000\000\000\000\039\000\000\000\000\000\
    \023\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\037\000\000\000\000\000\000\000\036\000\000\000\038\000\
    \000\000\000\000\000\000\031\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\000\000\000\000\000\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\000\000\255\255\000\000\000\000\046\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\003\000\005\000\000\000\004\000\044\000\
    \047\000\048\000\044\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\005\000\027\000\255\255\255\255\044\000\
    \000\000\050\000\006\000\051\000\255\255\044\000\255\255\044\000\
    \255\255\255\255\255\255\255\255\255\255\030\000\255\255\255\255\
    \255\255\255\255\030\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\000\000\255\255\000\000\027\000\255\255\
    \255\255\009\000\255\255\255\255\255\255\008\000\000\000\015\000\
    \018\000\010\000\020\000\021\000\014\000\017\000\000\000\030\000\
    \023\000\255\255\000\000\255\255\255\255\030\000\255\255\255\255\
    \022\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\030\000\255\255\255\255\255\255\030\000\255\255\030\000\
    \255\255\255\255\255\255\030\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\031\000\031\000\031\000\
    \031\000\031\000\031\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\042\000\042\000\042\000\042\000\
    \042\000\042\000\255\255\255\255\255\255\031\000\031\000\031\000\
    \031\000\031\000\031\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\042\000\042\000\042\000\042\000\
    \042\000\042\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\027\000\255\255\255\255\044\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 58 "src/ppx/exclude_lexer.mll"
                  ( Exclude_parser.CLOSING_BRACKET )
# 175 "src/ppx/exclude_lexer.ml"

  | 1 ->
# 59 "src/ppx/exclude_lexer.mll"
                  ( Exclude_parser.OPENING_BRACKET )
# 180 "src/ppx/exclude_lexer.ml"

  | 2 ->
# 60 "src/ppx/exclude_lexer.mll"
                  ( Exclude_parser.SEMICOLON )
# 185 "src/ppx/exclude_lexer.ml"

  | 3 ->
# 61 "src/ppx/exclude_lexer.mll"
                  ( Exclude_parser.FILE )
# 190 "src/ppx/exclude_lexer.ml"

  | 4 ->
# 62 "src/ppx/exclude_lexer.mll"
                  ( Exclude_parser.NAME )
# 195 "src/ppx/exclude_lexer.ml"

  | 5 ->
# 63 "src/ppx/exclude_lexer.mll"
                  ( Exclude_parser.REGEXP )
# 200 "src/ppx/exclude_lexer.ml"

  | 6 ->
# 64 "src/ppx/exclude_lexer.mll"
                  ( string 0 (Buffer.create 64) lexbuf )
# 205 "src/ppx/exclude_lexer.ml"

  | 7 ->
# 65 "src/ppx/exclude_lexer.mll"
                  ( comment 1 lexbuf )
# 210 "src/ppx/exclude_lexer.ml"

  | 8 ->
# 66 "src/ppx/exclude_lexer.mll"
                  ( token lexbuf )
# 215 "src/ppx/exclude_lexer.ml"

  | 9 ->
# 67 "src/ppx/exclude_lexer.mll"
                  ( incr_line lexbuf; token lexbuf )
# 220 "src/ppx/exclude_lexer.ml"

  | 10 ->
# 68 "src/ppx/exclude_lexer.mll"
                  ( Exclude_parser.EOF )
# 225 "src/ppx/exclude_lexer.ml"

  | 11 ->
let
# 69 "src/ppx/exclude_lexer.mll"
       ch
# 231 "src/ppx/exclude_lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 69 "src/ppx/exclude_lexer.mll"
                  ( fail lexbuf (Invalid_character ch) )
# 235 "src/ppx/exclude_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and string n strbuf lexbuf =
   __ocaml_lex_string_rec n strbuf lexbuf 27
and __ocaml_lex_string_rec n strbuf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 71 "src/ppx/exclude_lexer.mll"
                  ( Buffer.add_char strbuf '\008'; string n strbuf lexbuf )
# 247 "src/ppx/exclude_lexer.ml"

  | 1 ->
# 72 "src/ppx/exclude_lexer.mll"
                  ( Buffer.add_char strbuf '\009'; string n strbuf lexbuf )
# 252 "src/ppx/exclude_lexer.ml"

  | 2 ->
# 73 "src/ppx/exclude_lexer.mll"
                  ( Buffer.add_char strbuf '\010'; string n strbuf lexbuf )
# 257 "src/ppx/exclude_lexer.ml"

  | 3 ->
# 74 "src/ppx/exclude_lexer.mll"
                  ( Buffer.add_char strbuf '\013'; string n strbuf lexbuf )
# 262 "src/ppx/exclude_lexer.ml"

  | 4 ->
# 75 "src/ppx/exclude_lexer.mll"
                  ( Buffer.add_char strbuf '\''; string n strbuf lexbuf )
# 267 "src/ppx/exclude_lexer.ml"

  | 5 ->
# 76 "src/ppx/exclude_lexer.mll"
                  ( Buffer.add_char strbuf '\"'; string n strbuf lexbuf )
# 272 "src/ppx/exclude_lexer.ml"

  | 6 ->
# 77 "src/ppx/exclude_lexer.mll"
                  ( Buffer.add_char strbuf '\\'; string n strbuf lexbuf )
# 277 "src/ppx/exclude_lexer.ml"

  | 7 ->
let
# 78 "src/ppx/exclude_lexer.mll"
                o
# 283 "src/ppx/exclude_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 4) in
# 78 "src/ppx/exclude_lexer.mll"
                  ( add_octal_char strbuf o; string n strbuf lexbuf )
# 287 "src/ppx/exclude_lexer.ml"

  | 8 ->
let
# 79 "src/ppx/exclude_lexer.mll"
                h
# 293 "src/ppx/exclude_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 4) in
# 79 "src/ppx/exclude_lexer.mll"
                  ( add_hexa_char strbuf h; string n strbuf lexbuf )
# 297 "src/ppx/exclude_lexer.ml"

  | 9 ->
# 80 "src/ppx/exclude_lexer.mll"
                  ( if n = 0 then
                      Exclude_parser.STRING (Buffer.contents strbuf)
                    else
                      comment n lexbuf )
# 305 "src/ppx/exclude_lexer.ml"

  | 10 ->
let
# 84 "src/ppx/exclude_lexer.mll"
       c
# 311 "src/ppx/exclude_lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 84 "src/ppx/exclude_lexer.mll"
                  ( Buffer.add_char strbuf c; string n strbuf lexbuf )
# 315 "src/ppx/exclude_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_string_rec n strbuf lexbuf __ocaml_lex_state

and comment n lexbuf =
   __ocaml_lex_comment_rec n lexbuf 44
and __ocaml_lex_comment_rec n lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 86 "src/ppx/exclude_lexer.mll"
                  ( comment (succ n) lexbuf )
# 327 "src/ppx/exclude_lexer.ml"

  | 1 ->
# 87 "src/ppx/exclude_lexer.mll"
                  ( if n = 1 then token lexbuf else comment (pred n) lexbuf )
# 332 "src/ppx/exclude_lexer.ml"

  | 2 ->
# 88 "src/ppx/exclude_lexer.mll"
                  ( string n (Buffer.create 64) lexbuf )
# 337 "src/ppx/exclude_lexer.ml"

  | 3 ->
# 89 "src/ppx/exclude_lexer.mll"
                  ( incr_line lexbuf; comment n lexbuf )
# 342 "src/ppx/exclude_lexer.ml"

  | 4 ->
# 90 "src/ppx/exclude_lexer.mll"
                  ( fail lexbuf Unexpected_end_of_file )
# 347 "src/ppx/exclude_lexer.ml"

  | 5 ->
# 91 "src/ppx/exclude_lexer.mll"
                  ( comment n lexbuf )
# 352 "src/ppx/exclude_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_comment_rec n lexbuf __ocaml_lex_state

;;

