%{
open Ast
%}

%token <int> INT
%token <float> FLOAT
%token LPAR RPAR
%token PLUS
%token EOF EOL

%left PLUS

%start main
%type <Ast.any_exp> main

%%

main:
| exp EOL { $1 }
;

exp:
| INT           { Exp (Int $1) }
| FLOAT         { Exp (Float $1) }
| LPAR exp RPAR { $2 }
| exp PLUS exp  {
  let Exp e = $1 in
  let Exp f = $3 in
  match type_of e, type_of f with
  | TInt, TInt -> Exp (BinOp(e, plusi, f))
  | _          -> raise Type_error
}
;
