(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Asp.Unstaged

module Stream = struct
  include Stream

  let map f stream =
    let next _ =
      match Stream.next stream with
      | v -> Some (f v)
      | exception Stream.Failure -> None
    in
    Stream.from next

  let filter p stream =
    let rec next i =
      match Stream.next stream with
      | v -> (match p v with
              | Some v -> Some v
              | None -> next i)
      | exception Stream.Failure -> None
    in
    Stream.from next
end


module Util = struct
  let always x _ = x
  let some x = Some x
  let cons (x, xs) = x :: xs
end

(* A tag module for tokens *)
module Tok = struct
  type kwd = Let | In | If | Then | Else | Case | End | Fun
  type op = Plus | Minus | Times | Eql
  type 'a t =
    | Kwd : kwd -> unit t
    | Id : string t
    | Int : int t
    | Op : op -> unit t
    | LParen : unit t
    | RParen : unit t
    | LBrace : unit t
    | RBrace : unit t
    | Comma : unit t
    | Bar : unit t
    | Colon : unit t

  let int_of_kwd = function
    | Let  -> 0
    | In   -> 1
    | If   -> 2
    | Then -> 3
    | Else -> 4
    | Case -> 5
    | End  -> 6
    | Fun  -> 7

  let int_of_op = function
    | Plus -> 8
    | Minus -> 9
    | Times -> 10
    | Eql -> 11

  let int_of_t : type a. a t -> int = function
    | Kwd k -> int_of_kwd k
    | Op o  -> int_of_op o
    | Id    -> 11
    | Int   -> 12
    | LParen -> 13
    | RParen -> 14
    | LBrace -> 15
    | RBrace -> 16
    | Comma  -> 17
    | Bar    -> 18
    | Colon  -> 19

  let compare : type a b. a t -> b t -> (a,b) Asp.Types.cmp =
    fun t1 t2 ->
    match t1, t2 with
    | Kwd k1, Kwd k2 when k1 = k2 -> Eql
    | Id, Id                      -> Eql
    | Int, Int                    -> Eql
    | Op o1, Op o2 when o1 = o2   -> Eql
    | LParen, LParen -> Eql
    | RParen, RParen -> Eql
    | LBrace, LBrace -> Eql
    | Comma, Comma -> Eql
    | Bar, Bar -> Eql
    | Colon, Colon -> Eql
    | _ -> (match compare (int_of_t t1) (int_of_t t2) with
            | -1 -> Leq
            | 1 -> Geq
            | _ -> assert false)


  let print : type a. Format.formatter -> a t -> unit =
    fun out tag ->
    Format.fprintf out "%s"
      (match tag with
       | Kwd If -> "if"
       | Kwd Let -> "let"
       | Kwd In -> "in"
       | Kwd Then -> "then"
       | Kwd Else -> "else"
       | Kwd Case -> "case"
       | Kwd End  -> "end"
       | Kwd Fun  -> "fun"
       | Op Plus -> "+"
       | Op Minus -> "-"
       | Op Times -> "*"
       | Op Eql   -> "="
       | Id -> "<id>"
       | Int -> "<int>"
       | LParen -> "("
       | RParen -> ")"
       | LBrace -> "{"
       | RBrace -> "}"
       | Comma  -> ","
       | Bar -> "|"
       | Colon -> ":")

end

module Lexing = Parse(Asp.Utilities.Unstaged.Char_tag)
module Parsing = Parse(Tok)

module Lexer = struct
  module P = Lexing

  open P
  open Util

  let chr c = tok (Chr c) $ always c

  let token t v = Parsing.Tok(t, v)
  let token' t = token t ()
  let kwd_token k = token' (Tok.Kwd k)
  let op_token o = token' (Tok.Op o)

  let charset s =
    let len = String.length s in
    let rec loop i acc =
      if i < len then
        loop (i+1) (acc <|> chr s.[i])
      else
        acc
    in loop 0 bot

  let complement_string s' = (* assumes characters in s' are distinct *)
    let s = Bytes.make (256 - String.length s') '\000' in
    let j = ref 0 in
    for i = 0 to 255 do
      let c = Char.chr i in
      if not (String.contains s' c) then begin
          Bytes.set s (!j) c;
          incr j
        end
    done;
    Bytes.unsafe_to_string s

  let complement s = charset (complement_string s)

  let lower = charset "abcdefghijklmnopqrstuvwxyz"
  let upper = charset "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  let digit = charset "0123456789"
  let sym = charset "_'?"

  let kwd_or_id = lower >>> star (any [lower; upper; digit; sym])
           $ cons
           $ Asp.Utilities.Unstaged.Unstaged.toString
           $ (function
              | "let"   -> kwd_token Tok.Let
              | "in"    -> kwd_token Tok.In
              | "if"    -> kwd_token Tok.If
              | "then"  -> kwd_token Tok.Then
              | "else"  -> kwd_token Tok.Else
              | s       -> token Tok.Id s)
           $ some

  let int_of_char c = Char.code c - Char.code '0'
  let digits_to_int = List.fold_left (fun acc n -> n + 10 * acc) 0

  let int' = (digit >>> star digit)
            $ cons
            $ List.map int_of_char
            $ digits_to_int

  let int = int' $ (fun n -> Parsing.Tok(Tok.Int, n)) $ some

  let op c o = chr c $ always (op_token o)

  let ops =
    any [op '+' Tok.Plus;
         op '-' Tok.Minus;
         op '*' Tok.Times;
         op '=' Tok.Eql;]
    $ some

  let lparen = chr '(' $ always (token' Tok.LParen)
  let rparen = chr ')' $ always (token' Tok.RParen)
  let parens = any [lparen; rparen] $ some

  let ws' = any [chr ' '; chr '\t'; chr '\n'; chr '\r']
  let ws = ws' $ always None

  let token = type_check (any [ws; ops; int; kwd_or_id; parens])
  (* let tp_token = P.Grammar.typeof P.Grammar.[] token *)
  let p_token = P.parse token

  let stream_of_string s =
    Stream.map (fun c -> P.Tok(Chr c, c)) (Stream.of_string s)

  let tokenize_string p s =
    let s = stream_of_string s in
    let next _ =
      match Stream.peek s with
      | None -> None
      | Some _ -> Some(p s)
    in
    Stream.filter (fun x -> x) (Stream.from next)

  let lex s = tokenize_string p_token s
end

module Exp = struct
  module P = Parsing
  open P
  open Util
  open Tok

  type exp =
    | Var of string
    | Let of string * exp * exp
    | Int of int
    | Op of op * exp * exp
    | If of exp * exp * exp


  let (@@) f p = p $ f
  let (|>) p1 p2 = p1 >>> p2 $ (fun (f, x) -> f x)

  let any gs = List.fold_left (<|>) bot gs
  let paren p =
    (fun _ e _ -> e)
    @@ tok LParen |> p |> tok RParen
  let ident = tok Id $ fun x -> Var x
  let int = tok Int $ fun x -> Int x
  let kwd k = tok (Kwd k)
  let op o = tok (Op o)

  let mkOp o e1 e2 = Op(o, e1, e2)
  let parseOp o = op o $ always o

  let exp = fix (fun exp ->
                let atom = any [paren exp; ident; int] in
                let eqexp = infix mkOp atom [Left, parseOp Times;
                                             Left, any [parseOp Plus; parseOp Minus];
                                             Left, parseOp Eql]
                in
                let letexp =
                  (fun _ x _ e1 _ e2 -> Let(x, e1, e2))
                  @@ kwd Let |> tok Id |> op Eql |> exp |> kwd In |> exp in
                let ifexp =
                  (fun _ e1 _ e2 _ e3 -> If(e1, e2, e3))
                  @@ kwd If |> exp |> kwd Then |> exp |> kwd Else |> exp
                in
                any [eqexp; letexp; ifexp])

  let p_exp = P.parse (type_check exp)

  let parse_exp s = p_exp (Lexer.lex s)
end
