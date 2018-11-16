(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module P1 = Asp.Staged.Parse(Asp.Utilities.Unstaged.Char_element)
module P2 = Asp.Staged.Parse(Json_tokens)

module Combinator_examples = Test_int_exp

module Lexer = struct
  open P1
  open Asp.Utilities.Staged.Charparsing
         
  let plus c = c >>> star c $ fun p -> .< (fst .~p) :: (snd .~p) >.

  let decimal =
    plus digit >>>
      maybe ((chr '.') >>> plus digit) $ fun p -> .<
    match .~p with (x, None) -> Asp_utilities.Unstaged.toString x
               | (x, Some (_,y)) -> Asp_utilities.Unstaged.toString x ^"."^ Asp_utilities.Unstaged.toString y
   >.

  let dot = Combinator_examples.Lexer.complement ""

  let lex = 
    let open Json_tokens_base in
    fix @@ fun lex ->
      (chr '[' $ (fun _ -> .< Some (T (LBRACKET, ())) >.))
  <|> (chr ']' $ (fun _ -> .< Some (T (RBRACKET, ())) >.))
  <|> (chr '{' $ (fun _ -> .< Some (T (LBRACE, ())) >.))
  <|> (chr '}' $ (fun _ -> .< Some (T (RBRACE, ())) >.))
  <|> (chr ',' $ (fun _ -> .< Some (T (COMMA, ())) >.))
  <|> (chr ':' $ (fun _ -> .< Some (T (COLON, ())) >.))
  <|> (chr 'n' >>> 
       chr 'u' >>> 
       chr 'l' >>> 
       chr 'l' $ fun _ -> .< Some (T (NULL, ()))>.)
  <|> (chr 't' >>> 
       chr 'r' >>> 
       chr 'u' >>> 
       chr 'e' $ fun _ -> .<Some (T (TRUE, ()))>.)
  <|> (chr 'f' >>> 
       chr 'a' >>> 
       chr 'l' >>> 
       chr 's' >>> 
       chr 'e' $ fun _ -> .<Some (T (FALSE, ()))>.)
  <|> (string $ (fun s -> .<Some (T (STRING, .~s))>.))
  <|> (decimal $ (fun s -> .<Some (T (DECIMAL, .~s))>.))
  <|> (charset " \t\r\n" >>>
       lex $ fun p -> .< snd .~p >.)
  <|> eps .<None>.

  (* TODO: move all the following code to a library *)
  let lexcode =
    let module R = P1.Parser(Asp_streamcode.Stringcode) in
    R.compile (type_check lex)

  (* let () = Format.(fprintf err_formatter) "%a@." Print_code.print_code lexcode *)

  let staged_lexer = Runnative.run lexcode

  let next s =
    let i = ref 0 in
    fun _ ->
    let tok, i' = staged_lexer ~index:!i s in
    i := i';
    tok

  let staged_lexer_stream : string -> Json_tokens_base.t Stream.t =
    fun s -> Stream.from (next s)
end

module Parser =
struct
  open Json_tokens_base
  open P2

  let commasep p =
    fix @@ fun ps ->
    eps .<0>. <|> (p >>> maybe (tok COMMA >>> ps) $ fun p ->
    .< match .~p with
       | (x,None) -> x
       | (x,Some (_,xs)) -> x + xs >.)
  let delim l p r =
    (tok l >>> p >>> tok r) $ fun p -> .< let ((_,v),_) = .~p in  v >.

  (* Challenge: fixed points *)
  let value = fix @@ fun value ->
    let member = tok STRING >>>
                   maybe (tok COLON >>> value) $
                   fun p -> .< match .~p with  (_,None) -> 1
                                             | (_,Some(_,v)) -> 1 + v >. in
    let obj = delim LBRACE (commasep member) RBRACE
    and arr = delim LBRACKET (commasep value) RBRACKET
    in
    obj <|> arr <|> (tok STRING $ fun _ -> .<1>.) <|>
    (tok DECIMAL $ fun _ -> .<1>.) <|>
    (tok NULL $ fun _ -> .<1>.) <|>
    (tok TRUE $ fun _ -> .<1>.) <|>
    (tok FALSE $ fun _ -> .<1>.)
    
  let values = fix @@ fun values ->
    eps .<0>. <|> (value >>> values $ fun p -> .< fst .~p + snd .~p >.)

  (* TODO: move all the following code to a library *)
  let parsecode =
    let module R = P2.Parser(Asp_streamcode.Streamcode(struct type t = Json_tokens_base.t end)) in
    R.compile (type_check values)

  let staged_parser : Json_tokens_base.t Stream.t -> int
    = Runnative.run parsecode

  let staged_complete : string -> int =
    fun s -> staged_parser (Lexer.staged_lexer_stream s)
end
