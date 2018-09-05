(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module P1 = Asp.Staged.Parse(Asp.Utilities.Unstaged.Char_element)
module P2 = Asp.Staged.Parse(Pgn_tokens)

module Lexer = struct
  open P1
  open Asp.Utilities.Staged.Charparsing

  let int_or_int_dot =
    int >>> maybe (chr '.') $ fun p ->
   .< match .~p with (x, None) -> Some (Pgn_tokens_base.T (Pgn_tokens_base.INT, x))
                   | (x, Some _) -> Some (Pgn_tokens_base.T (Pgn_tokens_base.INTDOT, x)) >.

  let tag_or_coordinate_or_castle =
    ((upper <|> lower) >>>
     star (upper <|> lower <|> digit
           <|> chr '='
           <|> chr '#'
           <|> chr '-'
           <|> chr '+'))
     $ fun p ->
       let open Pgn_tokens_base in
       .< let l = fst .~p :: snd .~p in
          if List.exists (fun c -> '0' <= c && c <= '9') l then
            Some (T (COORDINATE, Asp.Utilities.Unstaged.Unstaged.toString l))
          else match Asp.Utilities.Unstaged.Unstaged.toString l with
         | "O-O" | "O-O#" | "O-O+" | "O-O-O" | "O-O-O#" | "O-O-O+" ->
            Some (T (CASTLE, ()))
         | s -> Some (T (TAG, s)) >.

  let lex : Pgn_tokens_base.t option t =
    let open Pgn_tokens_base in
    fix @@ fun self ->
             (chr '[' $ fun _ -> .< Some (T (LBRACKET, ())) >.)
           <|>
             (chr ']' $ fun _ -> .< Some (T (RBRACKET, ())) >.)
           <|>
             (chr '/' $ fun _ -> .< Some (T (SLASH, ())) >.)
           <|>
             (chr '-' $ fun _ -> .< Some (T (MINUS, ())) >.)
           <|>
             (chr '*' $ fun _ -> .< Some (T (STAR, ())) >.)
           <|>
             (string $ fun s -> .< Some (T (STRING, .~s)) >.)
           <|>
             tag_or_coordinate_or_castle
           <|>
             int_or_int_dot
           <|>
             ((whitespace >>> self) $
              fun x -> .< snd .~x >.)
           <|>
             (eps .<()>. $
              fun _ -> .< None >.)

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

  let staged_lexer_stream : string -> Pgn_tokens_base.t Stream.t =
    fun s -> Stream.from (next s)
end

module Parser =
struct
  open Pgn_tokens_base
  open P2
  module Utils = Asp.Utilities.Staged.Tokparsing(Pgn_tokens)
  open Utils

  let coordinate = (tok COORDINATE $ fun _ -> .<()>.)
                   <|> tok CASTLE

  let move = tok INTDOT >>> coordinate >>> maybe coordinate

  let result : [`DRAWN | `OTHER | `WON ] t =
    (tok STAR $ fun _ -> .<`OTHER>.)
    <|>
      ((tok INT >>>
         (((tok MINUS >>> tok INT) $ fun _ -> .<`WON>.)
          <|>
            ((tok SLASH >>> tok INT >>>
              tok MINUS >>>
              tok INT >>> tok SLASH >>> tok INT)
            $ fun _ -> .<`DRAWN>.)))
       $ fun p -> .< snd .~p >.)

  let moves = star move

  let metadatum =
    (tok LBRACKET >>> (tok TAG >>> tok STRING) >>> tok RBRACKET)
    $ fun p -> .< snd (fst .~p) >.

  let metadata = plus metadatum

  let game = (metadata >>> moves >>> result) $ fun p -> .<snd .~p>.

  let games =  plus game

  (* TODO: move all the following code to a library *)
  let parsecode =
    let module R = P2.Parser(Asp_streamcode.Streamcode(struct type t = Pgn_tokens_base.t end)) in
    R.compile (type_check games)

  let staged_parser : Pgn_tokens_base.t Stream.t -> [`DRAWN | `OTHER | `WON] list
    = Runnative.run parsecode

  let staged_complete : string -> [`DRAWN | `OTHER | `WON] list =
    fun s -> staged_parser (Lexer.staged_lexer_stream s)
end

