type token =
  | EOF
  | VAR of (string)
  | STRING of (string)
  | INT of (int)
  | BIN_MULT of (string)
  | BIN_PLUS of (string)
  | BIN_CMP of (string)
  | NOT
  | AND
  | OR
  | COMMA
  | LET
  | EQUALS
  | IN
  | MINUS
  | LPAR
  | RPAR

val terminated_expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expr.t
