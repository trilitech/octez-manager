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
# 13 "src/ppx/exclude_parser.mli"
)

val file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Exclude.file list
