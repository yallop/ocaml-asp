(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

include Sexp_tokens_base

let within_ : utag -> utag -> t code -> bool code =
  fun l h x ->
  let l = Sexp_tokens_base.to_int l and h = Sexp_tokens_base.to_int h in
  if l = h then .< Sexp_tokens_base.to_int (Sexp_tokens_base.tag .~x) = l >.
  else  .< l <= Sexp_tokens_base.to_int (Sexp_tokens_base.tag .~x) && Sexp_tokens_base.to_int (Sexp_tokens_base.tag .~x) <= h >.

let pp_utag fmt (U x) = Tag.print fmt x

let lift_tag : type a. a tag -> a tag code = function
  | ATOM   -> .< Sexp_tokens_base.ATOM   >.
  | LPAREN -> .< Sexp_tokens_base.LPAREN >.
  | RPAREN -> .< Sexp_tokens_base.RPAREN >.

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
 .< Sexp_tokens_base.match_ .~x .~(lift_tag tag)
                    (fun x -> .~(k (Some .<x>.)))
                    (fun () -> .~(k None)) >.
  | tags ->
 .< Sexp_tokens_base.matchlist_ .~x .~(lift_list lift_tag tags)
                    (fun x -> .~(k (Some .<x>.)))
                    (fun () -> .~(k None)) >.
