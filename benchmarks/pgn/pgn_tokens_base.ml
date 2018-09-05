(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type _ tag =
  | LBRACKET : unit tag
  | RBRACKET : unit tag
  | MINUS : unit tag
  | SLASH : unit tag
  | STAR : unit tag
  | CASTLE : unit tag
  | TAG : string tag
  | STRING : string tag
  | COORDINATE : string tag
  | INT : int tag
  | INTDOT : int tag

module Tag =
struct
  type 'a t = 'a tag

  let print : type a. Format.formatter -> a t -> unit =
    fun fmt -> function
    | LBRACKET   -> Format.fprintf fmt "LBRACKET"
    | RBRACKET   -> Format.fprintf fmt "RBRACKET"
    | MINUS      -> Format.fprintf fmt "MINUS"
    | SLASH      -> Format.fprintf fmt "SLASH"
    | STAR       -> Format.fprintf fmt "STAR"
    | CASTLE     -> Format.fprintf fmt "CASTLE"
    | TAG        -> Format.fprintf fmt "TAG"
    | STRING     -> Format.fprintf fmt "STRING"
    | COORDINATE -> Format.fprintf fmt "COORDINATE"
    | INT        -> Format.fprintf fmt "INT"
    | INTDOT     -> Format.fprintf fmt "INTDOT"

  let compare : type a b.a t -> b t -> (a, b) Asp.Types.cmp =
    fun x y ->
    match x, y with
    | LBRACKET   , LBRACKET   -> Eql
    | RBRACKET   , RBRACKET   -> Eql
    | MINUS      , MINUS      -> Eql
    | SLASH      , SLASH      -> Eql
    | STAR       , STAR       -> Eql
    | CASTLE     , CASTLE     -> Eql
    | TAG        , TAG        -> Eql
    | STRING     , STRING     -> Eql
    | COORDINATE , COORDINATE -> Eql
    | INT        , INT        -> Eql
    | INTDOT     , INTDOT     -> Eql
    | x     , y   -> if Obj.repr x < Obj.repr y then Leq else Geq
end

let format_tok : type a. Format.formatter -> (a tag * a) -> unit =
  fun ppf -> function
  | LBRACKET   , () -> Format.fprintf ppf "LBRACKET"
  | RBRACKET   , () -> Format.fprintf ppf "RBRACKET"
  | MINUS      , () -> Format.fprintf ppf "MINUS"
  | SLASH      , () -> Format.fprintf ppf "SLASH"
  | STAR       , () -> Format.fprintf ppf "STAR"
  | CASTLE     , () -> Format.fprintf ppf "CASTLE"
  | TAG        , s  -> Format.fprintf ppf "TAG %s" s
  | STRING     , s  -> Format.fprintf ppf "STRING %s" s
  | COORDINATE , s  -> Format.fprintf ppf "COORDINATE %s" s
  | INT        , i  -> Format.fprintf ppf "INT %d" i
  | INTDOT     , i  -> Format.fprintf ppf "INTDOT %d" i

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
            [U LBRACKET;
             U RBRACKET;
             U MINUS;
             U SLASH;
             U STAR;
             U CASTLE;
             U TAG;
             U STRING;
             U COORDINATE;
             U INT;
             U INTDOT]

let inj t = U t

let match_ : type a b. t -> a tag -> (a -> b) -> (unit -> b) -> b =
  fun (T (tag, v)) tag' yes no ->
  if U tag = U tag' then yes (Obj.magic v) else no ()

let matchlist_ : type a b. t -> a tag list -> (a -> b) -> (unit -> b) -> b =
  fun (T (tag, v)) tags yes no ->
  if List.mem (Obj.magic tag) tags then yes (Obj.magic v) else no ()
