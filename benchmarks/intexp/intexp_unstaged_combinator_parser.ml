(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module P1 = Asp.Unstaged.Parse(Asp.Utilities.Unstaged.Char_tag)
module P2 = Asp.Unstaged.Parse(Intexp_tokens_base.Tag)

module Combinator_examples = Test_int_exp

module Lexer = struct
  open P1

  let chr c = tok (Asp.Utilities.Unstaged.Chr c) $ fun _ -> c

  let kwd_or_id = Combinator_examples.Lexer.lower >>> star (any [Combinator_examples.Lexer.lower; Combinator_examples.Lexer.upper; Combinator_examples.Lexer.digit; Combinator_examples.Lexer.sym])
                  $ (fun s ->  match Asp.Utilities.Unstaged.Unstaged.toString (fst s :: snd s)  with
                                | "let"   -> Some (P2.Tok (Intexp_tokens_base.LET, ()))
                                | "in"    -> Some (P2.Tok (Intexp_tokens_base.IN, ()))
                                | "if"    -> Some (P2.Tok (Intexp_tokens_base.IF, ()))
                                | "then"  -> Some (P2.Tok (Intexp_tokens_base.THEN, ()))
                                | "else"  -> Some (P2.Tok (Intexp_tokens_base.ELSE, ()))
                                | s       -> Some (P2.Tok (Intexp_tokens_base.ID, s)))

  let int = Combinator_examples.Lexer.int' $ fun x ->  Some (P2.Tok (Intexp_tokens_base.INT, x))

  let lex : P2.token option t =
    fix @@ fun self ->
           (kwd_or_id
           <|>
            int
           <|>
             ((Combinator_examples.Lexer.ws' >>> self) $
              fun x ->  snd x )
           <|>
             (chr '(' $
              fun _ ->  Some (P2.Tok (Intexp_tokens_base.LPAREN, ())) )
           <|>
             (chr ')' $
              fun _ ->  Some (P2.Tok (Intexp_tokens_base.RPAREN, ())) )
           <|>
             (chr '+' $
              fun _ ->  Some (P2.Tok (Intexp_tokens_base.PLUS, ())) )
           <|>
             (chr '-' $
              fun _ ->  Some (P2.Tok (Intexp_tokens_base.MINUS, ())) )
           <|>
             (chr '*' $
              fun _ ->  Some (P2.Tok (Intexp_tokens_base.TIMES, ())) )
           <|>
             (chr '=' $
              fun _ ->  Some (P2.Tok (Intexp_tokens_base.EQUAL, ())) )
           <|>
             (eps () $
              fun _ ->  None ))

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
  open Intexp_tokens_base
  open P2

  type 'a term = (string * int) list -> 'a

  let paren p =
    (tok LPAREN >>> p >>> tok RPAREN)
      $ (fun p ->  snd (fst p) )

  let ident : int term t = tok ID $ fun x ->  List.assoc x
  let int : int term t = tok INT $ fun x ->  fun _ -> x

  let mkOp o e1 e2 =  fun env -> o (e1 env) (e2 env)

  let exp = fix (fun exp ->
                let atom = any [paren exp; ident; int] in
                let eqexp = infix mkOp atom
                              [Left, (tok TIMES $ fun _ ->  ( * ) );
                               Left, any [(tok PLUS  $ fun _ ->  ( + ) );
                                          (tok MINUS $ fun _ ->  ( - ) );];
                               Left, tok EQUAL $ fun _ ->  fun x y -> if (x = y) then 1 else 0  ]
                in
                let letexp =
                  (tok LET >>> tok ID >>> tok EQUAL >>> exp >>> tok IN >>> exp) $
                  (fun p ->  let (((((_,x),_),e),_),e') = p in
                               fun env -> e' ((x,e env) :: env) ) in
                let ifexp =
                  (tok IF >>> exp >>> tok THEN >>> exp >>> tok ELSE >>> exp) $
                  (fun p ->  let (((((_,e1),_),e2),_),e3) = p in
                               fun env -> if e1 env <> 0 then e2 env else e3 env)
                in
                any [eqexp; letexp; ifexp])

  let parser : P2.token Stream.t -> int term =
    P2.parse (P2.type_check exp)

  let unstaged_complete : string -> int term =
    fun s -> parser (Lexer.lexer_stream s)
end
