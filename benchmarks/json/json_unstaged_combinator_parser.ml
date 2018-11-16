(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module P1 = Asp.Unstaged.Parse(Asp.Utilities.Unstaged.Char_tag)
module P2 = Asp.Unstaged.Parse(Json_tokens_base.Tag)

module Combinator_examples = Test_int_exp

module Lexer = struct
  open P1

         
  let digit = Combinator_examples.Lexer.digit
  let plus c = c >>> star c $ fun (x,xs) -> x :: xs

  let chr c = tok (Asp.Utilities.Unstaged.Chr c) $ fun _ -> c

  let decimal =
    plus digit >>>
      maybe ((chr '.') >>> plus digit) $ fun p ->
    match p with (x, None) -> Asp_utilities.Unstaged.toString x
               | (x, Some (_,y)) -> Asp_utilities.Unstaged.toString x ^ "." ^
                                      Asp_utilities.Unstaged.toString y

  let dot = Combinator_examples.Lexer.complement ""

  let string =
    let open Asp.Utilities.Unstaged.Unstaged in
    (chr '"' >>> (star (Combinator_examples.Lexer.complement "\"\\"
                  <|> (chr '\\' >>> dot $ snd)))
     >>> chr '"')
    $ fun s -> toString (snd (fst s))

  let lex : P2.token option t = fix @@ fun lex ->
      (chr '[' $ (fun _ -> Some (P2.Tok (LBRACKET, ()))))
  <|> (chr ']' $ (fun _ -> Some (P2.Tok (RBRACKET, ()))))
  <|> (chr '{' $ (fun _ -> Some (P2.Tok (LBRACE, ()))))
  <|> (chr '}' $ (fun _ -> Some (P2.Tok (RBRACE, ()))))
  <|> (chr ',' $ (fun _ -> Some (P2.Tok (COMMA, ()))))
  <|> (chr ':' $ (fun _ -> Some (P2.Tok (COLON, ()))))
  <|> (chr 'n' >>> 
       chr 'u' >>> 
       chr 'l' >>> 
       chr 'l' $ fun _ -> Some (P2.Tok (NULL, ())))
  <|> (chr 't' >>> 
       chr 'r' >>> 
       chr 'u' >>> 
       chr 'e' $ fun _ -> Some (P2.Tok (TRUE, ())))
  <|> (chr 'f' >>> 
       chr 'a' >>> 
       chr 'l' >>> 
       chr 's' >>> 
       chr 'e' $ fun _ -> Some (P2.Tok (FALSE, ())))
  <|> (string $ (fun s -> Some (P2.Tok (STRING, s))))
  <|> (decimal $ (fun s -> Some (P2.Tok (DECIMAL, s))))
  <|> (Combinator_examples.Lexer.charset " \t\r\n" >>>
       lex $ fun (_,l) -> l)
  <|> eps None

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
  open Json_tokens_base
  open P2

  let commasep p =
    fix @@ fun ps ->
    eps 0 <|> (p >>> maybe (tok COMMA >>> ps) $ function
    | (x,None) -> x
    | (x,Some (_,xs)) -> x + xs)
  let delim l p r =
    (tok l >>> p >>> tok r) $ fun ((_,v),_) -> v

  (* Challenge: fixed points *)
  let value = fix @@ fun value ->
    let member = tok STRING >>>
                   maybe (tok COLON >>> value) $
                   function (_,None) -> 1
                          | (_,Some(_,v)) -> 1 + v in
    let obj = delim LBRACE (commasep member) RBRACE
    and arr = delim LBRACKET (commasep value) RBRACKET
    in
    obj <|> arr <|> (tok STRING $ fun _ -> 1) <|>
    (tok DECIMAL $ fun _ -> 1) <|>
    (tok NULL $ fun _ -> 1) <|>
    (tok TRUE $ fun _ -> 1) <|>
    (tok FALSE $ fun _ -> 1)
    
  let values = fix @@ fun values ->
    eps 0 <|> (value >>> values $ fun (x,xs) -> x + xs)

  let parser : P2.token Stream.t -> int =
    P2.parse (P2.type_check values)

  let unstaged_complete : string -> int =
    fun s -> parser (Lexer.lexer_stream s)
end
