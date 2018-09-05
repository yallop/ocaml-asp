(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module P1 = Asp.Unstaged.Parse(Asp.Utilities.Unstaged.Char_tag)
module P2 = Asp.Unstaged.Parse(Ppm_tokens_base.Tag)

module Combinator_examples = Test_int_exp

module Lexer = struct
  open P1

  let int = Combinator_examples.Lexer.int' $
              fun x -> Some (P2.Tok (Ppm_tokens_base.INT, x))

  let chr c = tok (Asp.Utilities.Unstaged.Chr c) $ fun _ -> c

  let whitespace = Combinator_examples.Lexer.ws'

  let lex : P2.token option t =
    fix @@ fun self ->
           ((chr 'P' >>> chr '3') $ fun _ ->  Some (P2.Tok (Ppm_tokens_base.P3, ())) )
           <|>
            int
           <|>
             ((whitespace >>> self) $
                fun x -> snd x)
           <|>
             ((chr '#' >>> Combinator_examples.Lexer.complement "\n") >>> self $
                fun x ->  snd x)
           <|>
             (eps None)

  let lexer_next : P1.token Stream.t -> P2.token option =
    P1.parse (P1.type_check lex)

  let next s _ = lexer_next s

  let lexer_stream : string -> P2.token Stream.t =
    fun s -> Stream.from (next
                            (Combinator_examples.Stream.map
                               (fun c -> P1.Tok(Chr c, c))
                               (Stream.of_string s)))
end

module Parser =
struct
  open P2
  open Ppm_tokens_base

  let int = tok INT

  let ints = fix @@ fun ints ->
                    (eps (min_int, 0))
                    <|>
                    (int >>> ints $ fun p ->  let (i, (m,count)) = p in
                                                (max i m, succ count) )

  let exp = (tok P3 >>> int >>> int >>> int >>> ints) $
              fun p ->  let (((((),w),h),max),(max',count)) = p in
                          (max' <= max) && (count = 3 * w * h)

  let parser : P2.token Stream.t -> bool =
    P2.parse (P2.type_check exp)

  let unstaged_complete : string -> bool =
    fun s -> parser (Lexer.lexer_stream s)
end
