type _ typ =
  | TInt  : int typ
  | TFloat: float typ

type ('a,'b,'c) binop = {
  name: string;
  f   : 'a -> 'b -> 'c;
  ty  : 'c typ;
}

let binop name f ty = { name; f; ty }

let plusi = binop "+" (+) TInt
let plusf = binop "+." (+.) TFloat

type _ exp =
  | Int  : int -> int exp
  | Float: float -> float exp
  | BinOp: 'a exp * ('a, 'b, 'c) binop * 'b exp -> 'c exp

type any_exp = Exp: _ exp -> any_exp

exception Type_error

let type_of: type t. t exp -> t typ = function
  | Int _         -> TInt
  | Float _       -> TFloat
  | BinOp (_,o,_) -> o.ty
