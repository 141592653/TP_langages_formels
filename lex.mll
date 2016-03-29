
{
  open Parse   (* ./parse.mly *)
  open Lexing  (* librairie standard *)
}



let varname = (['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*)
let intname = ['0'-'9'] ['0'-'9' '_']* | '-'? '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
    let strname = '"' ([^ '"'] | '\\' '"')* '"'

let bin_plus = '+' ['=' '<' '>' '^' '+' '*' '/' '%' '!' '$' '?' '.' ':' ';']*
  let bin_mult = ['*' '/' '%' '^'] ['=' '<' '>' '^' '+' '*' '/' '%' '!' '$' '?' '.' ':' ';']*
    let bin_cmp = ['<' '>'] ['=' '<' '>' '^' '+' '*' '/' '%' '!' '$' '?' '.' ':' ';']*
      let bin_min = '-' ['=' '<' '>' '^' '+' '*' '/' '%' '!' '$' '?' '.' ':' ';']*
	let bin_eq = '=' ['=' '<' '>' '^' '+' '*' '/' '%' '!' '$' '?' '.' ':' ';']*



      rule token = parse

      | "let"              { LET }
      | bin_eq                { EQUALS }
      | "in"               { IN }

      | "and"              { AND }
      | "or"               { OR }
      | "not"              { NOT }

      | varname as v       { VAR v }

      | strname as s       { STRING ( Scanf.unescaped(String.sub s 1 (String.length s - 2))) }
      | intname as i       { INT (int_of_string i) }

      | "("                { LPAR }
      | ")"                { RPAR }
      | ","                { COMMA }
      | bin_min            { MINUS }

      | bin_plus as p      { BIN_PLUS (String.make 1 p.[0]) }
      | bin_mult as m      { BIN_MULT (String.make 1 m.[0]) }
      | bin_cmp as c       { BIN_CMP (String.make 1 c.[0]) }
	  



      | ['\n']+   { token (lexbuf.lex_curr_p <- {
	lexbuf.lex_curr_p  with
	  pos_bol = lexbuf.lex_curr_p.pos_cnum ;
	  pos_lnum = 1 + lexbuf.lex_curr_p.pos_lnum } ;
      lexbuf) }
      |[' ' '\t' '\r' ] {token lexbuf}

      | eof                { EOF }

