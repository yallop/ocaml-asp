(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type _ tag =
  | LET : unit tag
  | IN : unit tag
  | IF : unit tag
  | THEN : unit tag
  | ELSE : unit tag
  | LPAREN : unit tag
  | RPAREN : unit tag
  | PLUS : unit tag
  | MINUS : unit tag
  | TIMES : unit tag
  | EQUAL : unit tag
  | INT : int tag
  | ID : string tag

module Tag =
struct
  type 'a t = 'a tag
  let print : type a. Format.formatter -> a t -> unit =
    fun fmt -> function
    | LET    -> Format.fprintf fmt "LET"
    | IN     -> Format.fprintf fmt "IN"
    | IF     -> Format.fprintf fmt "IF"
    | THEN   -> Format.fprintf fmt "THEN"
    | ELSE   -> Format.fprintf fmt "ELSE"
    | LPAREN -> Format.fprintf fmt "LPAREN"
    | RPAREN -> Format.fprintf fmt "RPAREN"
    | PLUS   -> Format.fprintf fmt "PLUS"
    | MINUS  -> Format.fprintf fmt "MINUS"
    | TIMES  -> Format.fprintf fmt "TIMES"
    | EQUAL  -> Format.fprintf fmt "EQUAL"
    | INT    -> Format.fprintf fmt "INT"
    | ID     -> Format.fprintf fmt "ID"

  let compare : type a b.a t -> b t -> (a, b) Asp.Types.cmp =
    fun x y ->
    match x, y with
    | LET    , LET    -> Eql
    | IN     , IN     -> Eql
    | IF     , IF     -> Eql
    | THEN   , THEN   -> Eql
    | ELSE   , ELSE   -> Eql
    | LPAREN , LPAREN -> Eql
    | RPAREN , RPAREN -> Eql
    | PLUS   , PLUS   -> Eql
    | MINUS  , MINUS  -> Eql
    | TIMES  , TIMES  -> Eql
    | EQUAL  , EQUAL  -> Eql
    | INT    , INT    -> Eql
    | ID     , ID     -> Eql
    | x      , y   -> if Obj.repr x < Obj.repr y then Leq else Geq
end

type utag = U : _ tag -> utag [@@unboxed]

type t = T : 'a tag * 'a -> t

let to_int : utag -> int = Obj.magic

let tag : t -> utag = function (T (tag,_)) -> U tag

module Ord =
struct
  type t = utag
  let compare l r = Pervasives.compare (to_int l) (to_int r)
end

module TagSet = Set.Make(Ord)
let all = TagSet.of_list
            [U LET;
             U IN;
             U IF;
             U THEN;
             U ELSE;
             U LPAREN;
             U RPAREN;
             U PLUS;
             U MINUS;
             U TIMES;
             U EQUAL;
             U INT;
             U ID]

let inj t = U t

let match_ : type a b. t -> a tag -> (a -> b) -> (unit -> b) -> b =
  fun (T (tag, v)) tag' yes no ->
  if U tag = U tag' then yes (Obj.magic v) else no ()

let matchlist_ : type a b. t -> a tag list -> (a -> b) -> (unit -> b) -> b =
  fun (T (tag, v)) tags yes no ->
  if List.mem (Obj.magic tag) tags then yes (Obj.magic v) else no ()
