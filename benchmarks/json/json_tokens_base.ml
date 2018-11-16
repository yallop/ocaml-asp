(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type _ tag =
  | LBRACKET : unit tag
  | RBRACKET : unit tag
  | LBRACE : unit tag
  | RBRACE : unit tag
  | COMMA : unit tag
  | COLON : unit tag
  | NULL : unit tag
  | TRUE : unit tag
  | FALSE : unit tag
  | STRING : string tag
  | DECIMAL : string tag

module Tag =
struct
  type 'a t = 'a tag

  let print : type a. Format.formatter -> a t -> unit =
    fun fmt -> function
    | LBRACKET -> Format.fprintf fmt "LBRACKET" 
    | RBRACKET -> Format.fprintf fmt "RBRACKET" 
    | LBRACE -> Format.fprintf fmt "LBRACE" 
    | RBRACE -> Format.fprintf fmt "RBRACE" 
    | COMMA -> Format.fprintf fmt "COMMA" 
    | COLON -> Format.fprintf fmt "COLON" 
    | NULL -> Format.fprintf fmt "NULL" 
    | TRUE -> Format.fprintf fmt "TRUE" 
    | FALSE -> Format.fprintf fmt "FALSE" 
    | STRING -> Format.fprintf fmt "STRING" 
    | DECIMAL -> Format.fprintf fmt "DECIMAL"

  let compare : type a b.a t -> b t -> (a, b) Asp.Types.cmp =
    fun x y ->
    match x, y with
    | LBRACKET , LBRACKET -> Eql
    | RBRACKET , RBRACKET -> Eql
    | LBRACE   , LBRACE -> Eql
    | RBRACE   , RBRACE -> Eql
    | COMMA    , COMMA -> Eql
    | COLON    , COLON -> Eql
    | NULL     , NULL -> Eql
    | TRUE     , TRUE -> Eql
    | FALSE    , FALSE -> Eql
    | STRING   , STRING -> Eql
    | DECIMAL  , DECIMAL -> Eql
    | x        , y   -> if Obj.repr x < Obj.repr y then Leq else Geq
end

let format_tok : type a. Format.formatter -> (a tag * a) -> unit =
  fun ppf -> function
  | LBRACKET , () -> Format.fprintf ppf "LBRACKET"
  | RBRACKET , () -> Format.fprintf ppf "RBRACKET"
  | LBRACE   , () -> Format.fprintf ppf "LBRACE"
  | RBRACE   , () -> Format.fprintf ppf "RBRACE"
  | COMMA    , () -> Format.fprintf ppf "COMMA"
  | COLON    , () -> Format.fprintf ppf "COLON"
  | NULL     , () -> Format.fprintf ppf "NULL"
  | TRUE     , () -> Format.fprintf ppf "TRUE"
  | FALSE    , () -> Format.fprintf ppf "FALSE"
  | STRING   , s  -> Format.fprintf ppf "STRING %s" s
  | DECIMAL  , s  -> Format.fprintf ppf "DECIMAL %s" s

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
             U LBRACE;
             U RBRACE;
             U COMMA;
             U COLON;
             U NULL;
             U TRUE;
             U FALSE;
             U STRING;
             U DECIMAL]

let inj t = U t

let match_ : type a b. t -> a tag -> (a -> b) -> (unit -> b) -> b =
  fun (T (tag, v)) tag' yes no ->
  if U tag = U tag' then yes (Obj.magic v) else no ()

let matchlist_ : type a b. t -> a tag list -> (a -> b) -> (unit -> b) -> b =
  fun (T (tag, v)) tags yes no ->
  if List.mem (Obj.magic tag) tags then yes (Obj.magic v) else no ()
