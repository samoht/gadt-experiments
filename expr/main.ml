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

open Ast

let show_type: type t. t typ -> string = function
  | TInt   -> "int"
  | TFloat -> "float"

let to_string: type t. t typ -> t -> string = function
  | TInt   -> string_of_int
  | TFloat -> string_of_float

let rec show_exp: type t. t exp -> string = function
  | Int i         -> string_of_int i
  | Float f       -> string_of_float f
  | BinOp (e,o,f) -> Printf.sprintf "(%s %s %s)" (show_exp e) o.name (show_exp f)

let rec eval: type t. t exp -> t = function
  | Int x        -> x
  | Float f      -> f
  | BinOp(e,o,f) -> o.f (eval e) (eval f)

let parse () =
  Parser.main Lexer.token (Lexing.from_channel stdin)

let _ =
  let Exp e = parse () in
  let ty = type_of e in
  Printf.printf "%s : %s = %s\n%!"
    (show_exp e)
    (show_type ty)
    (to_string ty (eval e))
