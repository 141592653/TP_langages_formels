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

open Parsing;;
let _ = parse_error;;
# 2 "parse.mly"
  open Expr
# 25 "parse.ml"
let yytransl_const = [|
    0 (* EOF *);
  263 (* NOT *);
  264 (* AND *);
  265 (* OR *);
  266 (* COMMA *);
  267 (* LET *);
  268 (* EQUALS *);
  269 (* IN *);
  270 (* MINUS *);
  271 (* LPAR *);
  272 (* RPAR *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
  258 (* STRING *);
  259 (* INT *);
  260 (* BIN_MULT *);
  261 (* BIN_PLUS *);
  262 (* BIN_CMP *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\003\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\000\000\001\000\003\000\001\000\001\000\001\000\003\000\
\006\000\004\000\003\000\003\000\003\000\003\000\003\000\002\000\
\003\000\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\007\000\005\000\000\000\000\000\000\000\
\000\000\020\000\000\000\000\000\000\000\000\000\016\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\008\000\012\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\010\000\000\000\004\000\000\000\
\000\000"

let yydgoto = "\002\000\
\010\000\025\000\026\000"

let yysindex = "\255\255\
\041\255\000\000\245\254\000\000\000\000\041\255\005\255\041\255\
\041\255\000\000\071\000\041\255\004\255\249\254\000\000\056\255\
\000\000\041\255\041\255\041\255\041\255\041\255\041\255\041\255\
\069\255\251\254\041\255\000\000\000\000\013\255\062\255\004\255\
\033\255\102\255\254\254\041\255\000\000\080\255\000\000\041\255\
\091\255"

let yyrindex = "\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\003\255\040\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\007\255\000\000\000\000\000\000\000\000\013\000\031\000\051\000\
\053\000\049\000\022\000\003\255\000\000\000\000\000\000\000\000\
\058\000"

let yygindex = "\000\000\
\000\000\006\000\240\255"

let yytablesize = 341
let yytable = "\001\000\
\006\000\018\000\019\000\012\000\027\000\014\000\011\000\018\000\
\019\000\020\000\037\000\013\000\011\000\015\000\016\000\023\000\
\018\000\024\000\002\000\039\000\000\000\015\000\003\000\029\000\
\030\000\031\000\032\000\033\000\034\000\035\000\013\000\000\000\
\038\000\000\000\000\000\000\000\018\000\019\000\020\000\019\000\
\021\000\003\000\004\000\005\000\023\000\041\000\024\000\006\000\
\014\000\000\000\017\000\007\000\018\000\000\000\008\000\009\000\
\000\000\009\000\000\000\018\000\019\000\020\000\000\000\021\000\
\022\000\018\000\019\000\023\000\000\000\024\000\017\000\028\000\
\018\000\019\000\020\000\024\000\021\000\022\000\036\000\000\000\
\023\000\000\000\024\000\018\000\019\000\020\000\000\000\021\000\
\022\000\000\000\000\000\023\000\040\000\024\000\018\000\019\000\
\020\000\000\000\021\000\022\000\000\000\000\000\023\000\000\000\
\024\000\018\000\019\000\020\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\024\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\006\000\006\000\006\000\000\000\
\006\000\006\000\006\000\000\000\006\000\006\000\006\000\000\000\
\006\000\011\000\011\000\000\000\011\000\011\000\011\000\000\000\
\011\000\011\000\011\000\015\000\011\000\015\000\015\000\015\000\
\000\000\015\000\015\000\015\000\000\000\015\000\013\000\013\000\
\013\000\000\000\013\000\013\000\000\000\000\000\013\000\019\000\
\019\000\019\000\000\000\000\000\019\000\000\000\000\000\019\000\
\014\000\014\000\014\000\017\000\017\000\014\000\018\000\017\000\
\014\000\018\000\017\000\009\000\018\000\000\000\009\000\000\000\
\000\000\009\000\018\000\019\000\020\000\000\000\021\000\022\000\
\000\000\000\000\023\000\000\000\024\000"

let yycheck = "\001\000\
\000\000\004\001\005\001\015\001\012\001\001\001\001\000\004\001\
\005\001\006\001\016\001\006\000\000\000\008\000\009\000\012\001\
\004\001\014\001\016\001\036\000\255\255\000\000\016\001\018\000\
\019\000\020\000\021\000\022\000\023\000\024\000\000\000\255\255\
\027\000\255\255\255\255\255\255\004\001\005\001\006\001\000\000\
\008\001\001\001\002\001\003\001\012\001\040\000\014\001\007\001\
\000\000\255\255\000\000\011\001\000\000\255\255\014\001\015\001\
\255\255\000\000\255\255\004\001\005\001\006\001\255\255\008\001\
\009\001\004\001\005\001\012\001\255\255\014\001\000\000\016\001\
\004\001\005\001\006\001\014\001\008\001\009\001\010\001\255\255\
\012\001\255\255\014\001\004\001\005\001\006\001\255\255\008\001\
\009\001\255\255\255\255\012\001\013\001\014\001\004\001\005\001\
\006\001\255\255\008\001\009\001\255\255\255\255\012\001\255\255\
\014\001\004\001\005\001\006\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\014\001\255\255\255\255\255\255\255\255\
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
\255\255\255\255\255\255\255\255\004\001\005\001\006\001\255\255\
\008\001\009\001\010\001\255\255\012\001\013\001\014\001\255\255\
\016\001\005\001\006\001\255\255\008\001\009\001\010\001\255\255\
\012\001\013\001\014\001\006\001\016\001\008\001\009\001\010\001\
\255\255\012\001\013\001\014\001\255\255\016\001\008\001\009\001\
\010\001\255\255\012\001\013\001\255\255\255\255\016\001\008\001\
\009\001\010\001\255\255\255\255\013\001\255\255\255\255\016\001\
\008\001\009\001\010\001\009\001\010\001\013\001\010\001\013\001\
\016\001\013\001\016\001\010\001\016\001\255\255\013\001\255\255\
\255\255\016\001\004\001\005\001\006\001\255\255\008\001\009\001\
\255\255\255\255\012\001\255\255\014\001"

let yynames_const = "\
  EOF\000\
  NOT\000\
  AND\000\
  OR\000\
  COMMA\000\
  LET\000\
  EQUALS\000\
  IN\000\
  MINUS\000\
  LPAR\000\
  RPAR\000\
  "

let yynames_block = "\
  VAR\000\
  STRING\000\
  INT\000\
  BIN_MULT\000\
  BIN_PLUS\000\
  BIN_CMP\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 49 "parse.mly"
             ( _1 )
# 210 "parse.ml"
               : Expr.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parse.mly"
      ([])
# 216 "parse.ml"
               : 'my_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 53 "parse.mly"
         ([_1])
# 223 "parse.ml"
               : 'my_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'my_list) in
    Obj.repr(
# 54 "parse.mly"
                       (_1::_3)
# 231 "parse.ml"
               : 'my_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 57 "parse.mly"
                                   ( Int _1 )
# 238 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parse.mly"
                                   ( Var _1 )
# 245 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "parse.mly"
                                   ( String _1 )
# 252 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 60 "parse.mly"
                                   ( _2 )
# 259 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 61 "parse.mly"
                                   ( Let (_2,_4,_6) )
# 268 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'my_list) in
    Obj.repr(
# 62 "parse.mly"
                                   ( App (_1,_3))
# 276 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "parse.mly"
                                   ( match (_1,_3) with 
    |(Int i1, Int i2) -> Int(i1+i2)
    |(_,_) -> App (_2,[_1;_3]) )
# 287 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "parse.mly"
                                   ( match (_1,_3) with 
    |(Int i1, Int i2) -> begin 
      match _2 with
      |"*" -> Int(i1*i2)
      |"/" -> Int(i1/i2)
      |"%" -> Int(i1 mod i2)
      |"^" -> Int(int_of_float (float_of_int i1 ** float_of_int i2))
      |_ -> failwith "Caractère non attendu dans BIN_MULT"
    end
    |(_,_)->App(_2,[_1;_3]) )
# 305 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "parse.mly"
                                   ( App (_2,[_1;_3]) )
# 314 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 79 "parse.mly"
                                   ( App ("=",[_1;_3]) )
# 322 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "parse.mly"
                                   ( match (_1,_3) with 
    |(Int i1, Int i2) -> Int(i1-i2)
    |(_,_) -> App ("-",[_1;_3]) )
# 332 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "parse.mly"
                                   ( match _2 with 
    |Int i1 -> Int(-i1)
    |_ -> App ("-",[_2]))
# 341 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "parse.mly"
                                   ( App ("and",[_1;_3]) )
# 349 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "parse.mly"
                                   ( App ("or",[_1;_3]) )
# 357 "parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "parse.mly"
                                   ( App ("not",[_2]) )
# 364 "parse.ml"
               : 'expr))
(* Entry terminated_expr *)
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
let terminated_expr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Expr.t)
