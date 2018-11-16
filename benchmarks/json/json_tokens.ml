(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

include Json_tokens_base

let within_ : utag -> utag -> t code -> bool code =
  fun l h x ->
  let l = Json_tokens_base.to_int l and h = Json_tokens_base.to_int h in
  if l = h then .< Json_tokens_base.to_int (Json_tokens_base.tag .~x) = l >.
  else  .< l <= Json_tokens_base.to_int (Json_tokens_base.tag .~x) && Json_tokens_base.to_int (Json_tokens_base.tag .~x) <= h >.

let pp_utag fmt (U x) = Tag.print fmt x

let lift_tag : type a. a tag -> a tag code = function
  | LBRACKET -> .< Json_tokens_base.LBRACKET >.
  | RBRACKET -> .< Json_tokens_base.RBRACKET >.
  | LBRACE   -> .< Json_tokens_base.LBRACE >.
  | RBRACE   -> .< Json_tokens_base.RBRACE >.
  | COMMA    -> .< Json_tokens_base.COMMA >.
  | COLON    -> .< Json_tokens_base.COLON >.
  | NULL     -> .< Json_tokens_base.NULL >.
  | TRUE     -> .< Json_tokens_base.TRUE >.
  | FALSE    -> .< Json_tokens_base.FALSE >.
  | STRING   -> .< Json_tokens_base.STRING >.
  | DECIMAL  -> .< Json_tokens_base.DECIMAL >.

let rec lift_list f = function
  | [] -> .< [] >.
  | x :: xs -> .< .~(f x) :: .~(lift_list f xs) >.

let test_tag : type a b. complete:bool -> a tag list -> t code ->
                    (a code option -> b code) ->
                    b code =
  fun ~complete tags x k ->
  if complete then k (Some .< snd (Obj.magic .~x) >.) else
  match tags with
    [tag] ->
 .< Json_tokens_base.match_ .~x .~(lift_tag tag)
                    (fun x -> .~(k (Some .<x>.)))
                    (fun () -> .~(k None)) >.
  | tags ->
 .< Json_tokens_base.matchlist_ .~x .~(lift_list lift_tag tags)
                    (fun x -> .~(k (Some .<x>.)))
                    (fun () -> .~(k None)) >.
