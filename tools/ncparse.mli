type token =
  | WORD of (string)
  | OPEN of (string)
  | CHAR of (char)
  | LPAREN
  | RPAREN
  | CLOSE
  | EOF
  | COMMA
  | SPACE
  | ATSIGN

val text :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
