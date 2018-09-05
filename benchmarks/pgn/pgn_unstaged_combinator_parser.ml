(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module P1 = Asp.Unstaged.Parse(Asp.Utilities.Unstaged.Char_tag)
module P2 = Asp.Unstaged.Parse(Pgn_tokens_base.Tag)

module Combinator_examples = Test_int_exp

module Lexer = struct
  open P1

  let chr c = tok (Asp.Utilities.Unstaged.Chr c) $ fun _ -> c

  let int_or_int_dot =
    Combinator_examples.Lexer.int' >>> maybe (chr '.') $ fun p ->
    match p with (x, None) -> Some (P2.Tok (Pgn_tokens_base.INT, x))
               | (x, Some _) -> Some (P2.Tok (Pgn_tokens_base.INTDOT, x))

  let string =
    let open Asp.Utilities.Unstaged.Unstaged in
    (chr '"' >>> star (Combinator_examples.Lexer.complement "\"") >>> chr '"')
    $ fun s -> toString (snd (fst s))


  let tag_or_coordinate_or_castle =
    ((Combinator_examples.Lexer.upper <|> Combinator_examples.Lexer.lower) >>>
     star (Combinator_examples.Lexer.upper <|> Combinator_examples.Lexer.lower <|> Combinator_examples.Lexer.digit
           <|> chr '='
           <|> chr '#'
           <|> chr '-'
           <|> chr '+'))
     $ fun p ->
       let open Pgn_tokens_base in
        let l = fst p :: snd p in
          if List.exists (fun c -> '0' <= c && c <= '9') l then
            Some (P2.Tok (COORDINATE, Asp.Utilities.Unstaged.Unstaged.toString l))
          else match Asp.Utilities.Unstaged.Unstaged.toString l with
         | "O-O" | "O-O#" | "O-O+" | "O-O-O" | "O-O-O#" | "O-O-O+" ->
           Some (P2.Tok (CASTLE, ()))
         | s ->  Some (P2.Tok (TAG, s))

  let lex : P2.token option t =
    let open Pgn_tokens_base in
    fix @@ fun self ->
             (chr '[' $ fun _ -> Some (P2.Tok (LBRACKET, ())))
           <|>
             (chr ']' $ fun _ -> Some (P2.Tok (RBRACKET, ())))
           <|>
             (chr '/' $ fun _ -> Some (P2.Tok (SLASH, ())))
           <|>
             (chr '-' $ fun _ -> Some (P2.Tok (MINUS, ())))
           <|>
             (chr '*' $ fun _ -> Some (P2.Tok (STAR, ())))
           <|>
             (string $ fun s -> Some (P2.Tok (STRING, s)))
           <|>
             tag_or_coordinate_or_castle
           <|>
             int_or_int_dot
           <|>
             ((Combinator_examples.Lexer.ws' >>> self) $
              fun x ->  snd x )
           <|>
             (eps () $
              fun _ ->  None )

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
  open Pgn_tokens_base
  open P2
  let plus g = (g >>> star g) $ fun (p,q) -> p :: q

  let coordinate = (tok COORDINATE $ fun _ -> ())
                   <|> tok CASTLE

  let move = tok INTDOT >>> coordinate >>> maybe coordinate

  let result : [`DRAWN | `OTHER | `WON ] t =
    (tok STAR $ fun _ -> `OTHER)
    <|>
      ((tok INT >>>
         (((tok MINUS >>> tok INT) $ fun _ -> `WON)
          <|>
            ((tok SLASH >>> tok INT >>>
              tok MINUS >>>
              tok INT >>> tok SLASH >>> tok INT)
            $ fun _ -> `DRAWN)))
       $ fun p ->  snd p )

  let moves = star move

  let metadatum =
    (tok LBRACKET >>> (tok TAG >>> tok STRING) >>> tok RBRACKET)
    $ fun p ->  snd (fst p)

  let metadata = plus metadatum

  let game = (metadata >>> moves >>> result) $ fun p -> snd p

  let games =  plus game

  let parser : P2.token Stream.t -> [`DRAWN | `OTHER | `WON ] list =
    P2.parse (P2.type_check games)

  let unstaged_complete : string -> [`DRAWN | `OTHER | `WON ] list =
    fun s -> parser (Lexer.lexer_stream s)
end
