(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module Parse (Token : Asp.Types.TOKEN) = struct

  module C = Set.Make(Token.Ord)

  module Tp = struct
    let print_set fmt cs =
      Format.fprintf fmt "{ ";
      C.iter (Token.pp_utag fmt) cs;
      Format.fprintf fmt "}"

    type t = { first : C.t; follow : C.t; null : bool; guarded : bool }

    let print fmt t =
      Format.fprintf fmt "{ null = %b; guarded = %b; first = %a; follow = %a }" t.null t.guarded print_set t.first print_set t.follow

    let show_tp () t =
      let b = Buffer.create 0 in
      let fmt = Format.formatter_of_buffer b in
      print fmt t;
      Format.pp_flush_formatter fmt;
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
    let tok c = { first = C.of_list c; follow = C.empty; null = false; guarded = true }
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
      Eps : 'a code -> ('ctx, 'a, 'd) t'
    | Seq : ('ctx, 'a, 'd) t * ('ctx, 'b, 'd) t -> ('ctx, 'a * 'b, 'd) t'
    | Tok : 'a Token.tag list -> ('ctx, 'a, 'd) t'
    | Bot : ('ctx, 'a, 'd) t'
    | Alt : ('ctx, 'a, 'd) t * ('ctx, 'a, 'd) t -> ('ctx, 'a, 'd) t'
    | Map : ('a code -> 'b code) * ('ctx, 'a, 'd) t -> ('ctx, 'b, 'd) t'
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
      | Tok tok -> (Tp.tok (List.map Token.inj tok), Tok tok)
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

  module Parser' = struct
    open Grammar

    module Tokstream_ir = Asp_stream_ir.Ir(Token)

    open Tokstream_ir

    type 'a t = 'a Tokstream_ir.t

    module ParseEnv = Env(struct type nonrec 'a t = 'a t end)

    type 'ctx parse_env = 'ctx ParseEnv.t =
        [] : unit parse_env
      | (::) : 'a t * 'ctx parse_env -> ('a * 'ctx) parse_env

    let bot _ = return .< failwith "Impossible" >.

    let alt tp1 p1 tp2 p2 =
      let open Tp in
      peek_mem tp1.first @@ function
      | `Eof when tp1.null -> p1
      | `Eof -> p2
      | `Yes -> p1
      | `No ->
         peek_mem tp2.first @@ function
         | `Eof when tp1.null -> p1
         | `Eof -> p2
         | `Yes -> p2
         | `No when tp1.null -> p1
         | `No when tp2.null -> p2
         | `No -> fail "No progress possible!"

    let tok c =
      peek c @@ function
      | `Eof -> fail "Expected chr"
      | `Yes c -> junk >>= fun _ -> return c
      | `No -> Printf.kprintf fail "wrong token"

    let seq p1 p2 =
      p1 >>= fun a ->
      p2 >>= fun b ->
      return .<(.~a, .~b)>.

    let map f p = p >>= fun x -> return (f x)

    let eps = return

    let star tp g =
      fix (fun loop ->
          peek_mem (C.diff Token.all tp.Tp.first) @@ function
          | `Eof -> return (.<[]>. : _ list code)
          | `Yes -> return (.<[]>. : _ list code)
          | `No -> g >>= fun x ->
                   loop >>= fun acc ->
                   return (.< .~x :: .~acc >. : _ list code))

    let crush : type ctx a x. (ctx, a, x) Grammar.t -> (ctx, a, x) Grammar.t =
      let rec loop (toks : a Token.tag list) (summands : (ctx, a, x) Grammar.t list) = function
        | _, Alt (l, r) ->
           let toks, summands = loop toks summands l in
           let toks, summands = loop toks summands r in
           (toks, summands)
        | _, Tok t -> (t @ toks, summands)
        | e -> (toks, e :: summands)
      in
      let alt e es = List.fold_right (fun (d,x) y -> Alt ((d,x), (d,y))) e es in
      fun (d,_ as e) ->
      d, match loop [] [] e with
         | [], [] -> Bot
         | toks, [] -> Tok toks
         | [], (_,e) :: es -> alt es e
         | toks, es -> alt es (Tok toks)

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
      | Fix g' -> fix (fun p -> parse g' (p :: penv))
  end

  module Parser (S: Asp.Types.STREAMCODE with type element = Token.t) =
  struct
    module M = Parser'.Tokstream_ir.Codegen(S)

    let compile g = M.generate (Parser'.(parse g ParseEnv.[]))
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
    let tok t = { untdb = fun _ -> (), Tok [t] }
    let bot = { untdb = fun _ -> (), Bot }
    let (<|>) f g = { untdb = fun i -> Parser'.crush ((), Alt (f.untdb i, g.untdb i)) }
    let any gs = List.fold_left (<|>) bot gs
    let ($) g f = { untdb = fun i -> (), Map (f, g.untdb i) }
    let fix f =
      { untdb = fun i ->
                let open Ctx in
                (), Fix ((f {untdb = fun j -> (), Var (tshift j (() :: i))}).untdb
                       (() :: i)) }
    let star g = { untdb = fun i -> (), Star (g.untdb i) }

    let maybe p = any [p $ (fun x -> .<Some .~x>.); eps .<None>.]

    type assoc = Left | Right

    let infixr f base op =
      fix (fun g -> base >>> maybe (op >>> g)
                    $ (fun x -> .< match .~x with
                       | (e, None) -> e
                       | (e, Some(o, e')) -> .~(f .<o>. .<e>. .<e'>.)>.))

    let infixl f base op =
      base >>> star (op >>> base)
      $ fun p -> .< let (e, oes) = .~p in
                     List.fold_left (fun e (o, e') -> .~(f .<o>. .<e>. .<e'>.)) e oes >.


    let mkInfix = function
      | Left -> infixl
      | Right -> infixr

    let infix f base aos =
      List.fold_left (fun base (a, o) -> mkInfix a f base o) base aos

    let type_check {untdb} = Grammar.typeof Grammar.TpEnv.[] (untdb Ctx.[])
  end

  include HOAS
end
