(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Asp

module Parse (Tag : Types.TAG) = struct
  module Token = struct
    type t = Tok : 'a Tag.t * 'a -> t
  end
  type token = Token.t = Tok : 'a Tag.t * 'a -> token

  module TokenType = struct
    type t = T : 'a Tag.t -> t

    let compare : t -> t -> int =
      fun (T t1) (T t2) ->
      match Tag.compare t1 t2 with
      | Types.Leq -> -1
      | Types.Eql -> 0
      | Types.Geq -> 1
  end

  module C = Set.Make(TokenType)

  module Tp = struct
    let print_set fmt cs =
      Format.fprintf fmt "{ ";
      C.iter (fun (TokenType.T tag) -> Tag.print fmt tag) cs;
      Format.fprintf fmt "}"

    type t = { first : C.t; follow : C.t; null : bool; guarded : bool }

    let print fmt t =
      Format.fprintf fmt "{ null = %b; guarded = %b; first = %a; follow = %a }" t.null t.guarded print_set t.first print_set t.follow

    let show_tp () t =
      let b = Buffer.create 0 in
      let fmt = Format.formatter_of_buffer b in
      print fmt t;
      Format.pp_print_flush fmt ();
      Buffer.contents b

    let show fmt = Format.sprintf fmt

    let equal t1 t2 =
      C.equal t1.first t2.first
      && C.equal t1.follow t2.follow
      && (t1.null = t2.null)
      && (t1.guarded = t2.guarded)

    let nonoverlapping cs cs' = C.is_empty (C.inter cs cs')
    let (++) = C.union
    let (==>) b cs = if b then cs else C.empty

    let apart t1 t2 = not t1.null && (nonoverlapping t1.follow t2.first)
    let disjoint t1 t2 = not (t1.null && t2.null) && nonoverlapping t1.first t2.first

    let check b msg = if b then () else failwith msg


    let eps = { first = C.empty; follow = C.empty; null = true; guarded = true }
    let tok c = { first = C.singleton c; follow = C.empty; null = false; guarded = true }
    let seq t1 t2 = check (apart t1 t2) (show "seq %a %a" show_tp t1 show_tp t2); {
          first = t1.first;
          follow = t2.follow ++ (t2.null ==> t2.first ++ t1.follow);
          null = false;
          guarded = t1.guarded
        }
    let bot = { first = C.empty; follow = C.empty; null = false; guarded = true }
    let alt t1 t2 = check (disjoint t1 t2) (show "alt %a %a" show_tp t1 show_tp t2); {
          first = t1.first ++ t2.first;
          follow = t1.follow ++ t2.follow;
          null = t1.null || t2.null;
          guarded = t1.guarded && t2.guarded;
        }
    let star t = {(seq t t) with null = true; follow = t.first ++ t.follow}

    let min = { first = C.empty; follow = C.empty; null = false; guarded = false }

    let fix f =
      let rec loop tp =
        let tp' = f tp in
        if equal tp tp' then
          tp
        else
          loop tp'
      in
      loop min

  end

  module Var = struct
    type ('ctx, 'a) t =
      | Z : ('a * 'rest, 'a) t
      | S : ('rest, 'a) t -> ('b * 'rest, 'a) t
  end

  module Env(T : sig type 'a t end) = struct
    type 'ctx t =
      | [] : unit t
      | (::) : 'a T.t * 'ctx t -> ('a * 'ctx) t

    let rec lookup : type ctx a. ctx t -> (ctx, a) Var.t -> a T.t =
      fun env n ->
      let open Var in
      match n, env with
      | Z, x :: _   -> x
      | S n, _ :: xs -> lookup xs n

    type fn = {f : 'a. 'a T.t -> 'a T.t}
    let rec map : type ctx. fn -> ctx t -> ctx t = 
      fun {f} ctx -> 
      match ctx with 
      | [] -> [] 
      | x :: xs -> f x :: map {f} xs                                   
  end

  module Grammar = struct
    type ('ctx, 'a) var = ('ctx, 'a) Var.t =
      | Z : ('a * 'rest, 'a) var
      | S : ('rest, 'a) var -> ('b * 'rest, 'a) var

    type ('ctx, 'a, 'd) t' =
      Eps : 'a -> ('ctx, 'a, 'd) t'
    | Seq : ('ctx, 'a, 'd) t * ('ctx, 'b, 'd) t -> ('ctx, 'a * 'b, 'd) t'
    | Tok : 'a Tag.t -> ('ctx, 'a, 'd) t'
    | Bot : ('ctx, 'a, 'd) t'
    | Alt : ('ctx, 'a, 'd) t * ('ctx, 'a, 'd) t -> ('ctx, 'a, 'd) t'
    | Map : ('a -> 'b) * ('ctx, 'a, 'd) t -> ('ctx, 'b, 'd) t'
    | Fix : ('a * 'ctx, 'a, 'd) t -> ('ctx, 'a, 'd) t'
    | Var : ('ctx,'a) var -> ('ctx, 'a, 'd) t'
    | Star : ('ctx, 'a, 'd) t -> ('ctx, 'a list, 'd) t'
    and ('ctx, 'a, 'd) t = 'd * ('ctx, 'a, 'd) t'

    module TpEnv = Env(struct type 'a t = Tp.t end)

    let data : type ctx a d. (ctx, a, d) t -> d = fun (d, _) -> d

    let rec typeof : type ctx a d. ctx TpEnv.t -> (ctx, a, d) t -> (ctx, a, Tp.t) t  =
      fun env (_, g) ->
      match g with
      | Eps v -> (Tp.eps, Eps v)
      | Seq (g1, g2) -> let guarded tp = {tp with Tp.guarded = true} in 
                        let g1 = typeof env g1 in
                        let g2 = typeof (TpEnv.map {TpEnv.f = guarded} env) g2 in
                        (Tp.seq (data g1) (data g2), Seq(g1, g2))
      | Tok tag -> (Tp.tok (TokenType.T tag), Tok tag)
      | Bot  -> (Tp.bot, Bot)
      | Alt (g1, g2) -> let g1 = typeof env g1 in
                        let g2 = typeof env g2 in
                        (Tp.alt (data g1) (data g2), Alt(g1, g2))
      | Map(f, g') -> let g' = typeof env g' in
                      (data g', Map(f, g'))
      | Star g -> let g = typeof env g in
                  (Tp.star (data g), Star g)
      | Fix g'  -> let open TpEnv in
                   let tp = Tp.fix (fun tp -> data (typeof (tp :: env) g')) in
                   Tp.check tp.Tp.guarded (Tp.show "guarded %a" Tp.show_tp tp);
                   let g' = typeof (tp :: env) g' in
                   (data g', Fix g')
      | Var n -> (TpEnv.lookup env n, Var n)
  end

  type 'a type_checked = (unit, 'a, Tp.t) Grammar.t

  module Parser = struct
    open Grammar

    type 'a t = (Token.t Stream.t -> 'a)

    module ParseEnv = Env(struct type nonrec 'a t = 'a t end)
    type 'ctx parse_env = 'ctx ParseEnv.t =
        [] : unit parse_env
      | (::) : 'a t * 'ctx parse_env -> ('a * 'ctx) parse_env

    let error fmt = Format.kasprintf failwith fmt

    let bot _ _s = error "Impossible"

    let alt tp1 p1 tp2 p2 =
      let open Tp in
      fun s -> match Stream.peek s with
      | None -> if tp1.null then p1 s else
                if tp2.null then p2 s else
                  error "Unexpected end of stream"
      | Some (Token.Tok(tag, _)) ->
         if C.mem (TokenType.T tag) tp1.first then
           p1 s
         else if C.mem (TokenType.T tag) tp2.first then
           p2 s
         else if tp1.null then
           p1 s
         else if tp2.null then
           p2 s
         else
           error "No progress possible -- saw token '%a'" Tag.print tag

    let tok : type a. a Tag.t -> a t =
      fun tag s ->
      match Stream.peek s with
      | None -> error "Expected token '%a', reached end of stream" Tag.print tag
      | Some (Token.Tok(tag', v)) ->
         (match Tag.compare tag tag' with
          | Types.Eql -> Stream.junk s; v
          | Types.Leq -> error "Expected token '%a', got '%a'" Tag.print tag Tag.print tag'
          | Types.Geq -> error "Expected token '%a', got '%a'" Tag.print tag Tag.print tag')

    let seq p1 p2 s =
      let a = p1 s in
      let b = p2 s in
      (a, b)

    let map f p s = f (p s)

    let eps v _s = v

    let star : type a. Tp.t -> a t -> a list t = fun tp g ->
      let rec loop s acc =
        match Stream.peek s with
        | None -> List.rev acc
        | Some (Token.Tok(c, _)) when not (C.mem (TokenType.T c) tp.Tp.first) -> List.rev acc
        | Some _ -> loop s (g s :: acc)
      in
      fun s -> loop s []

    let rec parse : type ctx a. (ctx, a, Tp.t) Grammar.t -> ctx ParseEnv.t -> a t =
      fun (_, g) penv ->
      match g with
      | Eps v -> eps v
      | Seq (g1, g2) -> let p1 = parse g1 penv in
                        let p2 = parse g2 penv in
                        seq p1 p2
      | Tok t -> tok t
      | Bot  -> bot ()
      | Alt (g1, g2) -> let p1 = parse g1 penv in
                        let p2 = parse g2 penv in
                        alt (data g1) p1 (data g2) p2
      | Map(f, g') -> let p = parse g' penv in map f p
      | Star g1 -> let p1 = parse g1 penv in
                   star (data g1) p1
      | Var n -> ParseEnv.lookup penv n
      | Fix g' -> let open ParseEnv in
                  let r = ref (fun s -> assert false) in 
                  let p s = !r s in 
                  let q = parse g' (p :: penv) in
                  r := q;
                  p
  end

  module HOAS = struct
    (* Higher-order abstract syntax front-end to the parser
     combinators *)
    open Grammar

    module Ctx = Env(struct type 'a t = unit end)

    let rec len : type n. n Ctx.t -> int = fun ctx ->
      let open Ctx in
      match ctx with
      | [] -> 0
      | () :: ctx -> 1 + len ctx

    let rec tshift' : type a i j. int -> j Ctx.t -> (a * i) Ctx.t -> (j, a) Var.t =
      fun n c1 c2 ->
      let open Ctx in
      match n, c1, c2 with
        (* Both the 'assert false' and the 'Obj.magic' are safe here,
           since we know (although it's not captured in the types) that
           (a * i) is a prefix of j.

           More details: "Unembedding Domain Specific Languages" §4.4.
         *)
      |        _, [], _ -> assert false
      | 0, () :: _, () :: _ -> Obj.magic Var.Z
      | n, () :: c1, c2 -> Var.S (tshift' (n-1) c1 c2)

    let tshift : type a i j. j Ctx.t -> (a * i) Ctx.t -> (j, a) Var.t =
      fun c1 c2 -> tshift' (len c1 - len c2) c1 c2

    type 'a t = { untdb : 'ctx. 'ctx Ctx.t -> ('ctx, 'a, unit) Grammar.t }

    let eps a = { untdb = fun _ -> (), Eps a }
    let (>>>) f g = { untdb = fun i -> (), Seq (f.untdb i, g.untdb i) }
    let tok t = { untdb = fun _ -> (), Tok t }
    let bot = { untdb = fun _ -> (), Bot }
    let (<|>) f g = { untdb = fun i -> (), Alt (f.untdb i, g.untdb i) }
    let any gs = List.fold_left (<|>) bot gs
    let ($) g f = { untdb = fun i -> (), Map (f, g.untdb i) }
    let fix f =
      { untdb = fun i ->
                let open Ctx in
                (), Fix ((f {untdb = fun j -> (), Var (tshift j (() :: i))}).untdb
                       (() :: i)) }
    let star g = { untdb = fun i -> (), Star (g.untdb i) }

    let maybe p = any [p $ (fun x -> Some x); eps None]

    type assoc = Left | Right

    let infixr f base op =
      fix (fun g -> base >>> maybe (op >>> g)
                    $ (function
                       | (e, None) -> e
                       | (e, Some(o, e')) -> f o e e'))

    let infixl f base op =
      base >>> star (op >>> base)
      $ fun (e, oes) -> List.fold_left (fun e (o, e') -> f o e e') e oes


    let mkInfix = function
      | Left -> infixl
      | Right -> infixr

    let infix f base aos =
      List.fold_left (fun base (a, o) -> mkInfix a f base o) base aos

    let type_check {untdb} = Grammar.typeof Grammar.TpEnv.[] (untdb Ctx.[])

    type 'a v = 'a
  end
  include HOAS

  let parse g = Parser.(parse g [])
end
