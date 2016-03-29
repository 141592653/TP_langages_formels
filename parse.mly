%{
  open Expr
%}

%token EOF

%token <string> VAR
%token <string> STRING
%token <int> INT

%token <string> BIN_MULT
%token <string> BIN_PLUS
%token <string> BIN_CMP

%token NOT
%token AND
%token OR
%token COMMA
%token LET EQUALS IN
%token MINUS


%token LPAR RPAR

%nonassoc IN
%nonassoc OR
%nonassoc AND
%nonassoc NOT
%nonassoc EQUALS
%nonassoc BIN_CMP
%left MINUS
%left BIN_PLUS
%left BIN_MULT
%nonassoc UMINUS





/* Les non-terminaux par lesquels l'analyse peut commencer,
 * et la donnée de leurs types. */

%start terminated_expr
%type <Expr.t> terminated_expr

%%

terminated_expr:
  | expr EOF { $1 }

my_list:
  |   {[]}
  | expr {[$1]}
  | expr COMMA my_list {$1::$3}

expr:
  | INT                            { Int $1 }
  | VAR                            { Var $1 }
  | STRING                         { String $1 }
  | LPAR expr RPAR                 { $2 }
  | LET VAR EQUALS expr IN expr    { Let ($2,$4,$6) }
  | VAR LPAR my_list RPAR          { App ($1,$3)}
  | expr BIN_PLUS expr             { match ($1,$3) with 
    |(Int i1, Int i2) -> Int(i1+i2)
    |(_,_) -> App ($2,[$1;$3]) }

  | expr BIN_MULT expr             { match ($1,$3) with 
    |(Int i1, Int i2) -> begin 
      match $2 with
      |"*" -> Int(i1*i2)
      |"/" -> Int(i1/i2)
      |"%" -> Int(i1 mod i2)
      |"^" -> Int(int_of_float (float_of_int i1 ** float_of_int i2))
      |_ -> failwith "Caractère non attendu dans BIN_MULT"
    end
    |(_,_)->App($2,[$1;$3]) }

  | expr BIN_CMP expr              { App ($2,[$1;$3]) }
  | expr EQUALS expr               { App ("=",[$1;$3]) }
  | expr MINUS expr                { match ($1,$3) with 
    |(Int i1, Int i2) -> Int(i1-i2)
    |(_,_) -> App ("-",[$1;$3]) }

  | MINUS expr %prec UMINUS        { match $2 with 
    |Int i1 -> Int(-i1)
    |_ -> App ("-",[$2])}

  | expr AND expr                  { App ("and",[$1;$3]) }
  | expr OR expr                   { App ("or",[$1;$3]) }
  | NOT expr                       { App ("not",[$2]) }
