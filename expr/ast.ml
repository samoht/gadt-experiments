(*
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
 *)

type _ typ =
  | TInt  : int typ
  | TFloat: float typ

type ('a,'b,'c) binop = {
  name: string;
  f   : 'a -> 'b -> 'c;
  ty  : 'c typ;
}

let binop name f ty = { name; f; ty }

let plus_int   = binop "+" (+)  TInt
let plus_float = binop "+" (+.) TFloat

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
