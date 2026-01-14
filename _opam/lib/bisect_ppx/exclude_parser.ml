type token =
  | CLOSING_BRACKET
  | OPENING_BRACKET
  | SEMICOLON
  | FILE
  | NAME
  | REGEXP
  | EOF
  | STRING of (
# 34 "src/ppx/exclude_parser.mly"
        string
# 13 "src/ppx/exclude_parser.ml"
)

open Parsing
let _ = parse_error;;
# 8 "src/ppx/exclude_parser.mly"

type error =
  | Invalid_file_contents
  | Invalid_file_declaration
  | Invalid_exclusion
  | Invalid_regular_expression of string

let string_of_error = function
  | Invalid_file_contents -> "invalid file contents"
  | Invalid_file_declaration -> "invalid file declaration"
  | Invalid_exclusion -> "invalid exclusion"
  | Invalid_regular_expression re -> Printf.sprintf "invalid regular expression %S" re

let fail error =
  let pos = Parsing.symbol_start_pos () in
  let line = pos.Lexing.pos_lnum in
  raise (Exclude.Exception (line, string_of_error error))

let make_regexp s =
  try Str.regexp s
  with _ -> fail (Invalid_regular_expression s)

# 41 "src/ppx/exclude_parser.ml"
let yytransl_const = [|
  257 (* CLOSING_BRACKET *);
  258 (* OPENING_BRACKET *);
  259 (* SEMICOLON *);
  260 (* FILE *);
  261 (* NAME *);
  262 (* REGEXP *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  263 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\004\000\004\000\
\005\000\005\000\007\000\007\000\008\000\008\000\008\000\006\000\
\006\000\000\000"

let yylen = "\002\000\
\002\000\001\000\000\000\002\000\004\000\002\000\001\000\002\000\
\000\000\003\000\000\000\002\000\003\000\003\000\001\000\000\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\018\000\000\000\000\000\001\000\004\000\
\006\000\000\000\007\000\000\000\008\000\011\000\000\000\000\000\
\017\000\005\000\015\000\010\000\000\000\000\000\012\000\000\000\
\000\000\013\000\014\000"

let yydgoto = "\002\000\
\004\000\005\000\008\000\012\000\015\000\018\000\016\000\023\000"

let yysindex = "\011\000\
\005\255\000\000\000\000\000\000\002\000\000\255\000\000\000\000\
\000\000\012\255\000\000\018\255\000\000\000\000\019\255\008\255\
\000\000\000\000\000\000\000\000\014\255\016\255\000\000\019\255\
\019\255\000\000\000\000"

let yyrindex = "\000\000\
\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\004\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\010\255\
\010\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\249\255\000\000\000\000"

let yytablesize = 264
let yytable = "\009\000\
\009\000\007\000\003\000\016\000\003\000\010\000\011\000\019\000\
\020\000\016\000\016\000\001\000\021\000\022\000\016\000\016\000\
\026\000\027\000\013\000\014\000\024\000\017\000\025\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\009\000\009\000\006\000\003\000\016\000"

let yycheck = "\000\001\
\000\000\000\000\000\000\000\000\000\001\006\001\007\001\000\001\
\001\001\000\001\001\001\001\000\005\001\006\001\005\001\006\001\
\024\000\025\000\007\001\002\001\007\001\003\001\007\001\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\004\001\004\001\004\001\004\001"

let yynames_const = "\
  CLOSING_BRACKET\000\
  OPENING_BRACKET\000\
  SEMICOLON\000\
  FILE\000\
  NAME\000\
  REGEXP\000\
  EOF\000\
  "

let yynames_block = "\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'file_decl_list) in
    Obj.repr(
# 41 "src/ppx/exclude_parser.mly"
                                 ( List.rev _1 )
# 182 "src/ppx/exclude_parser.ml"
               : Exclude.file list))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "src/ppx/exclude_parser.mly"
                                 ( fail Invalid_file_contents )
# 188 "src/ppx/exclude_parser.ml"
               : Exclude.file list))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "src/ppx/exclude_parser.mly"
                                 ( [] )
# 194 "src/ppx/exclude_parser.ml"
               : 'file_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'file_decl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'file_decl) in
    Obj.repr(
# 45 "src/ppx/exclude_parser.mly"
                                 ( _2 :: _1 )
# 202 "src/ppx/exclude_parser.ml"
               : 'file_decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'file_pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exclusion_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'separator_opt) in
    Obj.repr(
# 48 "src/ppx/exclude_parser.mly"
                                 ( { Exclude.path = _2;
                                     Exclude.exclusions = _3; } )
# 212 "src/ppx/exclude_parser.ml"
               : 'file_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "src/ppx/exclude_parser.mly"
                                 ( fail Invalid_file_declaration )
# 218 "src/ppx/exclude_parser.ml"
               : 'file_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "src/ppx/exclude_parser.mly"
                                 ( Exclude.Name _1 )
# 225 "src/ppx/exclude_parser.ml"
               : 'file_pattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "src/ppx/exclude_parser.mly"
                                 ( Exclude.Regexp (make_regexp _2) )
# 232 "src/ppx/exclude_parser.ml"
               : 'file_pattern))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "src/ppx/exclude_parser.mly"
                                 ( None )
# 238 "src/ppx/exclude_parser.ml"
               : 'exclusion_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exclusions) in
    Obj.repr(
# 59 "src/ppx/exclude_parser.mly"
                                 ( Some (List.rev _2) )
# 245 "src/ppx/exclude_parser.ml"
               : 'exclusion_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "src/ppx/exclude_parser.mly"
                                 ( [] )
# 251 "src/ppx/exclude_parser.ml"
               : 'exclusions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exclusions) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exclusion) in
    Obj.repr(
# 63 "src/ppx/exclude_parser.mly"
                                 ( _2::_1 )
# 259 "src/ppx/exclude_parser.ml"
               : 'exclusions))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'separator_opt) in
    Obj.repr(
# 66 "src/ppx/exclude_parser.mly"
                                 ( Exclude.Name _2 )
# 267 "src/ppx/exclude_parser.ml"
               : 'exclusion))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'separator_opt) in
    Obj.repr(
# 67 "src/ppx/exclude_parser.mly"
                                 ( Exclude.Regexp (make_regexp _2) )
# 275 "src/ppx/exclude_parser.ml"
               : 'exclusion))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "src/ppx/exclude_parser.mly"
                                 ( fail Invalid_exclusion )
# 281 "src/ppx/exclude_parser.ml"
               : 'exclusion))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "src/ppx/exclude_parser.mly"
                                 ( )
# 287 "src/ppx/exclude_parser.ml"
               : 'separator_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "src/ppx/exclude_parser.mly"
                                 ( )
# 293 "src/ppx/exclude_parser.ml"
               : 'separator_opt))
(* Entry file *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let file (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Exclude.file list)
;;
