/*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

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
  | TInt  , TInt   -> Exp (BinOp(e, plus_int  , f))
  | TFloat, TFloat -> Exp (BinOp(e, plus_float, f))
  | _     , _      -> raise Type_error
}
;
