(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

include Intexp_tokens_base

let within_ : utag -> utag -> t code -> bool code =
  fun l h x ->
  let l = Intexp_tokens_base.to_int l and h = Intexp_tokens_base.to_int h in
  if l = h then .< Intexp_tokens_base.to_int (Intexp_tokens_base.tag .~x) = l >.
  else  .< l <= Intexp_tokens_base.to_int (Intexp_tokens_base.tag .~x) && Intexp_tokens_base.to_int (Intexp_tokens_base.tag .~x) <= h >.

let pp_utag fmt (U x) = Tag.print fmt x

let lift_tag : type a. a tag -> a tag code = function
  | LET    -> .< Intexp_tokens_base.LET >.
  | IN     -> .< Intexp_tokens_base.IN >.
  | IF     -> .< Intexp_tokens_base.IF >.
  | THEN   -> .< Intexp_tokens_base.THEN >.
  | ELSE   -> .< Intexp_tokens_base.ELSE >.
  | LPAREN -> .< Intexp_tokens_base.LPAREN >.
  | RPAREN -> .< Intexp_tokens_base.RPAREN >.
  | PLUS   -> .< Intexp_tokens_base.PLUS >.
  | MINUS  -> .< Intexp_tokens_base.MINUS >.
  | TIMES  -> .< Intexp_tokens_base.TIMES >.
  | EQUAL  -> .< Intexp_tokens_base.EQUAL >.
  | INT    -> .< Intexp_tokens_base.INT >.
  | ID     -> .< Intexp_tokens_base.ID >.

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
 .< Intexp_tokens_base.match_ .~x .~(lift_tag tag)
                    (fun x -> .~(k (Some .<x>.)))
                    (fun () -> .~(k None)) >.
  | tags ->
 .< Intexp_tokens_base.matchlist_ .~x .~(lift_list lift_tag tags)
                    (fun x -> .~(k (Some .<x>.)))
                    (fun () -> .~(k None)) >.
