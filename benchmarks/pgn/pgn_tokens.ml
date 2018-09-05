(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

include Pgn_tokens_base

let within_ : utag -> utag -> t code -> bool code =
  fun l h x ->
  let l = Pgn_tokens_base.to_int l and h = Pgn_tokens_base.to_int h in
  if l = h then .< Pgn_tokens_base.to_int (Pgn_tokens_base.tag .~x) = l >.
  else  .< l <= Pgn_tokens_base.to_int (Pgn_tokens_base.tag .~x) && Pgn_tokens_base.to_int (Pgn_tokens_base.tag .~x) <= h >.

let pp_utag fmt (U x) = Tag.print fmt x

let lift_tag : type a. a tag -> a tag code = function
  | LBRACKET   -> .< Pgn_tokens_base.LBRACKET >.
  | RBRACKET   -> .< Pgn_tokens_base.RBRACKET >.
  | MINUS      -> .< Pgn_tokens_base.MINUS >.
  | SLASH      -> .< Pgn_tokens_base.SLASH >.
  | STAR       -> .< Pgn_tokens_base.STAR >.
  | CASTLE     -> .< Pgn_tokens_base.CASTLE >.
  | TAG        -> .< Pgn_tokens_base.TAG >.
  | STRING     -> .< Pgn_tokens_base.STRING >.
  | COORDINATE -> .< Pgn_tokens_base.COORDINATE >.
  | INT        -> .< Pgn_tokens_base.INT >.
  | INTDOT     -> .< Pgn_tokens_base.INTDOT >.

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
 .< Pgn_tokens_base.match_ .~x .~(lift_tag tag)
                    (fun x -> .~(k (Some .<x>.)))
                    (fun () -> .~(k None)) >.
  | tags ->
 .< Pgn_tokens_base.matchlist_ .~x .~(lift_list lift_tag tags)
                    (fun x -> .~(k (Some .<x>.)))
                    (fun () -> .~(k None)) >.
