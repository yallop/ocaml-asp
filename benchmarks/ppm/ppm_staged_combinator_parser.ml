(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module P1 = Asp.Staged.Parse(Asp.Utilities.Unstaged.Char_element)
module P2 = Asp.Staged.Parse(Ppm_tokens)

module Lexer = struct
  open P1
  open Asp.Utilities.Staged.Charparsing

  let int = int $ fun x -> .< Some (Ppm_tokens_base.T (Ppm_tokens_base.INT, .~x)) >.

  let lex : Ppm_tokens_base.t option t =
    fix @@ fun self ->
           ((chr 'P' >>> chr '3') $ fun _ -> .< Some (Ppm_tokens_base.T (Ppm_tokens_base.P3, ())) >.)
           <|>
            int
           <|>
             ((whitespace >>> self) $
              fun x -> .< snd .~x >.)
           <|>
             ((chr '#' >>> complement "\n") >>> self $
               fun x -> .< snd .~x >.)
           <|>
             (eps .<None>.)

  (* TODO: move all the following code to a library *)
  let lexcode =
    let module R = P1.Parser(Asp_streamcode.Stringcode) in
    R.compile (type_check lex)

  let staged_lexer = Runnative.run lexcode

  let next s =
    let i = ref 0 in
    fun _ ->
    let tok, i' = staged_lexer ~index:!i s in
    i := i';
    tok

  let staged_lexer_stream : string -> Ppm_tokens_base.t Stream.t =
    fun s -> Stream.from (next s)
end

module Parser =
struct
  open P2
  open Ppm_tokens_base

  let int = tok INT

  let ints = fix @@ fun ints ->
                    (eps .< (min_int, 0) >.)
                    <|>
                    (int >>> ints $ fun p -> .< let (i, (m,count)) = .~p in
                                                (max i m, succ count) >.)

  let exp = (tok P3 >>> int >>> int >>> int >>> ints) $
              fun p -> .< let (((((),w),h),max),(max',count)) = .~p in
                          (max' <= max) && (count = 3 * w * h) >.

  (* TODO: move all the following code to a library *)
  let parsecode =
    let module R = P2.Parser(Asp_streamcode.Streamcode(struct type t = Ppm_tokens_base.t end)) in
    R.compile (type_check exp)

  let staged_parser : Ppm_tokens_base.t Stream.t -> bool
    = Runnative.run parsecode

  let staged_complete : string -> bool =
    fun s -> staged_parser (Lexer.staged_lexer_stream s)
end
