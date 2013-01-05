{
  open Parser
}

let int = '-'? ['0'-'9']+
let float = int ['.'] int?
let space = [' ' '\t' '\r' '\n']

rule token = parse
| space  { token lexbuf }
| "("    { LPAR }
| ")"    { RPAR }
| "+"    { PLUS }
| int    { INT (int_of_string (Lexing.lexeme lexbuf)) }
| float  { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
| eof    { EOF }
| _      { let token = Lexing.lexeme lexbuf in
           Printf.eprintf "lexer error: '%s' is not a valid token\n" token;
           raise Parsing.Parse_error }
