(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module Tokens (E: Asp.Types.TOKEN) =
struct
  module S = Set.Make(E.Ord)
  type t = S.t
  type elt = E.t
  type 'a sd = Sta of 'a | Dyn of 'a code

  type interval = E.utag * E.utag

  (* Divide a set into fully-populated intervals. *)
  let intervals : S.t -> interval list =
    let add c = function
      | (l,h) :: tail when E.to_int c = succ (E.to_int h) -> (l, c) :: tail
      | l -> (c, c) :: l
    in fun c -> List.rev (S.fold add c [])

  (* Build a dynamic test from an interval test *)
  let within : elt code -> interval -> bool code =
    fun c (l, h) -> E.within_ l h c

  let member : elt code -> S.t -> bool sd =
    fun c s -> match intervals s with
        [] -> Sta false
      | i :: is ->
        Dyn (List.fold_left (fun a i -> .< .~a || .~(within c i) >.)
               (within c i) is)

  let ntests s =
    List.fold_left
      (fun v (l, h) -> v + if E.Ord.compare l h = 0 then 1 else 2)
      0
      (intervals s)

  let ifmem : 'a.
        elt code -> S.t -> S.t ->
        then_:(S.t -> 'a code) ->
        else_:(S.t -> 'a code) -> 'a code
    = fun c values cs ~then_ ~else_ ->
    (* if c ∈ cs
       then [c is in thencs]
       else [c is in elsecs] *)
    let thencs, elsecs = S.partition
        (fun c -> S.mem c cs) values
    in
    if S.is_empty elsecs then then_ thencs
    else if S.is_empty thencs then else_ elsecs
    else
      let thencs, elsecs, then_, else_ =
        if ntests thencs < ntests elsecs
        then thencs, elsecs, then_, else_
        else elsecs, thencs, else_, then_
      in
      match member c thencs with
        Sta _ -> assert false (* :-( *)
      | Dyn test ->
        .< if .~test then
            .~(then_ thencs)
          else
            .~(else_ elsecs) >.
end

module Ir(E: Asp.Types.TOKEN) =
struct
  module Toks = Tokens(E)

  type _ recid = ..

  (** Computations in normal form. *)
  type 'a comp =
    (* let x = r () in c[x] *)
    | RecCall : 'a recvar * ('a code -> 'b comp) -> 'b comp
    (* junk; c *)
    | Junk : 'b comp -> 'b comp
    (* match read with Eof → kEof | c when c ∈ s → kYes | _ → kNo *)
    | Peek_mem : Toks.t * ([`Eof | `Yes | `No] -> 'b comp) -> 'b comp
    (* match read with Eof → kEof
                     | c when hastag(c,t) → kYes[extract t c]
                     | _ → kNo *)
    | Peek : 'a E.tag list * ([`Eof | `Yes of 'a code | `No] -> 'b comp) -> 'b comp
    (* fail s *)
    | Fail : string -> _ comp
    (* return x *)
    | Return : 'a code -> 'a comp
  and 'a recvar = { body: 'a comp -> 'a comp; mutable name: 'a recid list }
  (* Stream.mkcall option *)

  (** binding maintains normal form.
      (warning: binding conditionals/reads duplicates continuations) *)
  let rec bind : type a b. a comp -> (a code -> b comp) -> b comp =
    fun m k -> match m with
    (* let z = (let x = r () in c[x]) in c'[z]
         ↝ let x = r () in
           let z = c[x] in c'[z]  *)
    | RecCall (r, k') -> RecCall (r, fun x -> bind (k' x) k)
    (* let z = (junk; c) in c'[z]
         ↝ junk; let z = c in c'[z]  *)
    | Junk c -> Junk (bind c k)
    (* let z = (match read with Eof → kEof | c when c ∈ s → kYes | _ → kNo) in c'[z]
         ↝ match read with
           | Eof → let z = kEof in c'[z]
           | c when c ∈ s → let z = kYes in c'[z]
           | _ → let z = kNo in c'[z] *)
    | Peek_mem (cs, k') -> Peek_mem (cs, fun v -> bind (k' v) k)
    (* let z = (match read with Eof → kEof
                              | c when hastag(c,t) → kYes[extract t c]
                              | _ → kNo) in c'[z]
         ↝ match read with Eof → let z = kEof in c'[z]
                         | c when hastag(c,t) → let z = kYes[extract t c] in c'[z]
                         | _ → let z = kNo in c'[z] *)
    | Peek (tag, k') -> Peek (tag, fun v -> bind (k' v) k)
    (* let z = (fail s) in c'
         ↝ fail s  *)
    | Fail s -> Fail s
    (* let z = (return x) in c'
         ↝ c'[x]  *)
    | Return x -> k x

  type 'a t = 'a comp
  let return v = Return v
  let (>>=) = bind
  let fail s = Fail s
  let junk = Junk (Return .<()>.)
  let peek_mem set k = Peek_mem (set, k)
  let peek tag k = Peek (tag, k)
  let fix body = RecCall ({ body; name = [] }, fun v -> Return v)

  module Codegen(Stream: Asp.Types.STREAMCODE with type element = E.t) =
  struct
    type context =
      { stream_context: Stream.context;
        values: Toks.t;
        next: [`EOF|`Tok of E.t code|`Unknown];
        rec_locus: Genletrec.locus_t; }

    type 'a recid += R of 'a Stream.mkcall
    let rec resolve = function
        [] -> None
      | R x :: _ -> Some x
      | _ :: xs -> resolve xs

    let rec cdcomp : type a. context -> a comp -> a Stream.return code =
      fun ctxt -> function
      | RecCall ({ name; body } as r, k') as c ->
         begin
           match resolve name with
           | Some {Stream.mkcall} ->
              mkcall ctxt.stream_context @@ fun stream_context x ->
              cdcomp { ctxt with stream_context; values = E.all; next = `Unknown }
                (k' x)
           | None ->
             let () = Stream.genfun ctxt.rec_locus
                       (fun stream_context mkcall ->
                         r.name <- R mkcall :: r.name;
                         let context = {ctxt with values = E.all;
                                                  next = `Unknown;
                                                  stream_context}
                         in
                         let self = RecCall (r, fun v -> Return v) in
                         cdcomp context (body self)) in
             cdcomp ctxt c
         end
      | Junk c ->
         Stream.junk ctxt.stream_context @@ fun stream_context ->
         let ctxt' = {ctxt with stream_context;
                                next = `Unknown;
                                values = E.all} in
         cdcomp ctxt' c
      | Peek_mem (s, k') ->
         begin match ctxt.next with
         | `EOF -> cdcomp ctxt (k' `Eof)
         | `Tok x ->
            Toks.ifmem x ctxt.values s
              ~then_:(fun values ->
                cdcomp {ctxt with values} (k' `Yes))
              ~else_:(fun values ->
                cdcomp {ctxt with values} (k' `No))
         | `Unknown ->
            Stream.peek ctxt.stream_context @@ fun stream_context -> function
            | None -> cdcomp {ctxt with stream_context; next = `EOF} (k' `Eof)
            | Some x ->
               let ctxt' = {ctxt with stream_context; next = `Tok x} in
               Toks.ifmem x ctxt'.values s
                    ~then_:(fun values -> cdcomp {ctxt' with values} (k' `Yes))
                    ~else_:(fun values -> cdcomp {ctxt' with values} (k' `No))
         end
      | Peek (tags, k') ->
         let utagset = Toks.S.of_list (List.map E.inj tags) in
         begin match ctxt.next with
         | `EOF -> cdcomp ctxt (k' `Eof)
         | `Tok x ->
            (* Check what we know already *)
            if Toks.S.(is_empty (inter ctxt.values utagset))
            then (* definitely not in there *) cdcomp ctxt (k' `No)
            else begin
                let complete = Toks.S.(subset ctxt.values utagset) in
                (* maybe in there *)
                let tags' = List.filter (fun tag -> Toks.S.mem (E.inj tag) ctxt.values) tags in
                E.test_tag ~complete tags' x @@ function
                | None -> cdcomp {ctxt with values = Toks.S.(diff ctxt.values utagset)} (k' `No)
                | Some x -> cdcomp {ctxt with values = Toks.S.of_list (List.map E.inj tags') } (k' (`Yes x))
              end
         | `Unknown ->
            Stream.peek ctxt.stream_context @@ fun stream_context -> function
            | None -> cdcomp {ctxt with stream_context; next = `EOF} (k' `Eof)
            | Some x ->
               E.test_tag ~complete:false tags x @@ function
               | None -> cdcomp {ctxt with values = Toks.S.diff ctxt.values utagset} (k' `No)
               | Some x -> cdcomp {ctxt with values = Toks.S.inter ctxt.values utagset} (k' (`Yes x))
         end
      | Fail s ->
         .< failwith s >.
      | Return x ->
         Stream.return ctxt.stream_context x

    let cdprog expr =
      Genletrec.genletrec_locus @@ fun rec_locus ->
       Stream.init @@ fun stream_context ->
       let context = {
             stream_context;
             next = `Unknown;
             values = E.all;
             rec_locus; } in
       cdcomp context expr

    let generate = cdprog
  end
end
