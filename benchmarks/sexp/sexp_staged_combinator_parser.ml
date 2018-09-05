(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module P1 = Asp.Staged.Parse(Asp.Utilities.Unstaged.Char_element)
module P2 = Asp.Staged.Parse(Sexp_tokens)

module Lexer =
struct
  open P1

  let chr c = tok (Asp.Utilities.Unstaged.Chr c)

  let charset s =
    let len = String.length s in
    let rec loop i acc =
      if i < len then
        loop (i+1) (acc <|> chr s.[i])
      else
        acc
    in loop 0 bot

  let lower = charset "abcdefghijklmnopqrstuvwxyz"
  let upper = charset "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  let digit = charset "0123456789"
  let whitespace = charset " \t\r\n"

  let lex =
    fix @@ fun self ->
           ((any [lower; upper] >>> star (any [lower; upper; digit])) $
              fun x -> .< Some (Sexp_tokens_base.T (Sexp_tokens_base.ATOM, fst .~x :: snd .~x)) >.)
           <|>
             ((whitespace >>> self) $
              fun x -> .< snd .~x >.)
           <|>
             (chr '(' $
              fun _ -> .< Some (Sexp_tokens_base.T (Sexp_tokens_base.LPAREN, ())) >.)
           <|>
             (chr ')' $
              fun _ -> .< Some (Sexp_tokens_base.T (Sexp_tokens_base.RPAREN, ())) >.)
           <|>
             (eps .<None>.)

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

  let staged_lexer_stream : string -> Sexp_tokens_base.t Stream.t =
    fun s -> Stream.from (next s)
end

module Parser =
struct
  open P2

  let parse =
    let open Sexp_tokens_base in
    fix @@ fun sexp ->
     ((tok LPAREN >>> star sexp >>> tok RPAREN) $ fun x -> .<List.fold_left (+) 0 (snd (fst .~x))  >.)
     <|>
      (tok ATOM $ fun _ -> .<1>.)

  let parsecode =
    let module R = P2.Parser(Asp_streamcode.Streamcode(struct type t = Sexp_tokens_base.t end)) in
    R.compile (type_check parse)

  let staged_parser : Sexp_tokens_base.t Stream.t -> int
    = Runnative.run parsecode

  let staged_complete : string -> int =
    fun s -> staged_parser (Lexer.staged_lexer_stream s)
end
