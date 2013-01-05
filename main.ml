open Ast

let show_type: type t. t typ -> string = function
  | TInt   -> "int"
  | TFloat -> "float"

let to_string: type t. t typ -> t -> string = function
  | TInt   -> string_of_int
  | TFloat -> string_of_float

let rec eval: type t. t exp -> t = function
  | Int x        -> x
  | Float f      -> f
  | BinOp(e,o,f) -> o.f (eval e) (eval f)

let rec show_exp: type t. t exp -> string = function
  | Int i         -> string_of_int i
  | Float f       -> string_of_float f
  | BinOp (e,o,f) -> Printf.sprintf "(%s %s %s)" (show_exp e) o.name (show_exp f)

let parse () =
  Parser.exp Lexer.token (Lexing.from_channel stdin)

let f s =
  let Exp e = parse () in
  let ty = type_of e in
  Printf.printf "%s : %s = %s\n"
    (show_exp e)
    (show_type ty)
    (to_string ty (eval e))
