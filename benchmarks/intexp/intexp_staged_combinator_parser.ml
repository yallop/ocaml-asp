(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module P1 = Asp.Staged.Parse(Asp.Utilities.Unstaged.Char_element)
module P2 = Asp.Staged.Parse(Intexp_tokens)

module Lexer = struct
  open P1
  open Asp.Utilities.Staged.Charparsing

  let kwd_or_id = lower >>> star (any [lower; upper; digit; sym])
                  $ (fun s -> .< match Asp.Utilities.Unstaged.Unstaged.toString (fst .~s :: snd .~s)  with
                                | "let"   -> Some (Intexp_tokens_base.T (Intexp_tokens_base.LET, ()))
                                | "in"    -> Some (Intexp_tokens_base.T (Intexp_tokens_base.IN, ()))
                                | "if"    -> Some (Intexp_tokens_base.T (Intexp_tokens_base.IF, ()))
                                | "then"  -> Some (Intexp_tokens_base.T (Intexp_tokens_base.THEN, ()))
                                | "else"  -> Some (Intexp_tokens_base.T (Intexp_tokens_base.ELSE, ()))
                                | s       -> Some (Intexp_tokens_base.T (Intexp_tokens_base.ID, s)) >.)

  let int = int $ fun x -> .< Some (Intexp_tokens_base.T (Intexp_tokens_base.INT, .~x)) >.

  let lex : Intexp_tokens_base.t option t =
    fix @@ fun self ->
           (kwd_or_id
           <|>
            int
           <|>
             ((whitespace >>> self) $
              fun x -> .< snd .~x >.)
           <|>
             (chr '(' $
              fun _ -> .< Some (Intexp_tokens_base.T (Intexp_tokens_base.LPAREN, ())) >.)
           <|>
             (chr ')' $
              fun _ -> .< Some (Intexp_tokens_base.T (Intexp_tokens_base.RPAREN, ())) >.)
           <|>
             (chr '+' $
              fun _ -> .< Some (Intexp_tokens_base.T (Intexp_tokens_base.PLUS, ())) >.)
           <|>
             (chr '-' $
              fun _ -> .< Some (Intexp_tokens_base.T (Intexp_tokens_base.MINUS, ())) >.)
           <|>
             (chr '*' $
              fun _ -> .< Some (Intexp_tokens_base.T (Intexp_tokens_base.TIMES, ())) >.)
           <|>
             (chr '=' $
              fun _ -> .< Some (Intexp_tokens_base.T (Intexp_tokens_base.EQUAL, ())) >.)
           <|>
             (eps .<()>. $
              fun _ -> .< None >.))

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

  let staged_lexer_stream : string -> Intexp_tokens_base.t Stream.t =
    fun s -> Stream.from (next s)
end

module Parser =
struct
  open Intexp_tokens_base
  open P2

  type 'a term = (string * int) list -> 'a

  let paren p =
    (tok LPAREN >>> p >>> tok RPAREN)
      $ (fun p -> .< snd (fst .~p) >.)

  let ident : int term t = tok ID $ fun x -> .< List.assoc .~x >.
  let int : int term t = tok INT $ fun x -> .< fun _ -> .~x >.

  let mkOp o e1 e2 = .< fun env -> .~o (.~e1 env) (.~e2 env) >.

  let exp = fix (fun exp ->
                let atom = any [paren exp; ident; int] in
                let eqexp = infix mkOp atom
                              [Left, (tok TIMES $ fun _ -> .< ( * ) >.);
                               Left, any [(tok PLUS  $ fun _ -> .< ( + ) >.);
                                          (tok MINUS $ fun _ -> .< ( - ) >.);];
                               Left, tok EQUAL $ fun _ -> .< fun x y -> if (x = y) then 1 else 0 >. ]
                in
                let letexp =
                  (tok LET >>> tok ID >>> tok EQUAL >>> exp >>> tok IN >>> exp) $
                  (fun p -> .< let (((((_,x),_),e),_),e') = .~p in
                               fun env -> e' ((x,e env) :: env) >.) in
                let ifexp =
                  (tok IF >>> exp >>> tok THEN >>> exp >>> tok ELSE >>> exp) $
                  (fun p -> .< let (((((_,e1),_),e2),_),e3) = .~p in
                               fun env -> if e1 env <> 0 then e2 env else e3 env>.)
                in
                any [eqexp; letexp; ifexp])

  (* TODO: move all the following code to a library *)
  let parsecode =
    let module R = P2.Parser(Asp_streamcode.Streamcode(struct type t = Intexp_tokens_base.t end)) in
    R.compile (type_check exp)

  let staged_parser : Intexp_tokens_base.t Stream.t -> int term
    = Runnative.run parsecode

  let staged_complete : string -> int term =
    fun s -> staged_parser (Lexer.staged_lexer_stream s)
end
