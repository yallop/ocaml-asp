(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module P1 = Asp.Unstaged.Parse(Asp.Utilities.Unstaged.Char_tag)

module Combinator_examples = Test_int_exp

module Lexer =
struct
  open P1

  let chr c = tok (Asp.Utilities.Unstaged.Chr c) $ fun _ -> c

  let charset s =
    let len = String.length s in
    let rec loop i acc =
      if i < len then
        loop (i+1) (acc <|> chr s.[i])
      else
        acc
    in loop 0 bot

  let ws = fix @@ ws -> (eps ()
                     <|> (chr '\x09' >>> ws) $ ignore
                     <|> (chr '\x0a' >>> ws) $ ignore
                     <|> (chr '\x0d' >>> ws) $ ignore
                     <|> (chr '\x20' >>> ws) $ ignore)

  let sign = eps () <|> (chr "+" $ ignore) <|> (chr "-" $ ignore)

  let exp = eps ()
        <|> (chr "E" >>> sign >>> digits $ ignore)
        <|> (chr "e" >>> sign >>> digits $ ignore)

  let frac = eps 0
        <|> (chr "." >>> digits) $ ignore

  let onenine = charset "123456789" 
  let digit = chr '0' <|> onenine

  let digits = plus digit

  let int = digit
        <|> (onenine >>> digits)
        <|> (chr '-' >>> digit)
        <|> (chr '-' >>> onenine >>> digits)

  let number = int >>> frac >>> exp

  let hex = digit <|> charset "abcdefABCDEF"

  let escape = charset "\"\\/bnrt" <|> (chr 'u' >>> hex >>> hex >>> hex >>> hex)

  let character = assert false  (* TODO
character
    '0020' . '10ffff' - '"' - '\'
    '\' escape
                                 *)

  let characters = star character

  let string = chr '"' >>> characters >>> chr '"'

  let element = ws >>> value >>> ws


  let elements = assert false (* TODO
elements
    element
    element ',' elements
                               *)

array
    '[' ws ']'
    '[' elements ']'


member
    ws string ws ':' element

members
    member
    member ',' members


object
    '{' ws '}'
    '{' members '}'


value
    object
    array
    string
    number
    "true"
    "false"
    "null"


json
    element

  let lower = charset "abcdefghijklmnopqrstuvwxyz"
  let upper = charset "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  let digit = charset "0123456789"
  let whitespace = charset " \t\r\n"

  let lex =
    fix @@ fun self ->
           ((any [lower; upper] >>> star (any [lower; upper; digit])) $
              fun x -> Some (P2.Tok (Sexp_tokens_base.ATOM, fst x :: snd x)) )
           <|>
             ((whitespace >>> self) $
              fun x -> snd x )
           <|>
             (chr '(' $
              fun _ -> Some (P2.Tok (Sexp_tokens_base.LPAREN, ())) )
           <|>
             (chr ')' $
              fun _ -> Some (P2.Tok (Sexp_tokens_base.RPAREN, ())) )
           <|>
             (eps None)

  let lexer_next : P1.token Stream.t -> P2.token option =
    P1.parse (P1.type_check lex)

  let next s _ = lexer_next s

  let lexer_stream : string -> P2.token Stream.t =
    fun s -> Stream.from (next (Combinator_examples.Stream.map
                                  (fun c -> P1.Tok (Chr c, c))
                                  (Stream.of_string s)))
end

module Parser =
struct
  open P2

  let parse =
    let open Sexp_tokens_base in
    fix @@ fun sexp ->
     ((tok LPAREN >>> star sexp >>> tok RPAREN) $ fun x -> List.fold_left (+) 0 (snd (fst x)))
     <|>
      (tok ATOM $ fun _ -> 1)

  let parser : P2.token Stream.t -> int =
    P2.parse (P2.type_check parse)

  let unstaged_complete : string -> int =
    fun s -> parser (Lexer.lexer_stream s)
end
