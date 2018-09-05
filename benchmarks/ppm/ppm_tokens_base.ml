(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type _ tag =
  | P3 : unit tag
  | INT : int tag

module Tag =
struct
  type 'a t = 'a tag

  let print : type a. Format.formatter -> a t -> unit =
    fun fmt -> function
     | P3  -> Format.fprintf fmt "P3"
     | INT -> Format.fprintf fmt "INT"

  let compare : type a b.a t -> b t -> (a, b) Asp.Types.cmp =
    fun x y ->
    match x, y with
    | P3    , P3  -> Eql
    | INT   , INT -> Eql
    | x     , y   -> (assert (Obj.repr x <> Obj.repr y); if Obj.repr x < Obj.repr y then Leq else Geq)
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
            [U P3;
             U INT]

let inj t = U t

let match_ : type a b. t -> a tag -> (a -> b) -> (unit -> b) -> b =
  fun (T (tag, v)) tag' yes no ->
  if U tag = U tag' then yes (Obj.magic v) else no ()

let matchlist_ : type a b. t -> a tag list -> (a -> b) -> (unit -> b) -> b =
  fun (T (tag, v)) tags yes no ->
  if List.mem (Obj.magic tag) tags then yes (Obj.magic v) else no ()
