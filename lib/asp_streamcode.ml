(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Asp.Types

module type S = STREAMCODE

module Streamcode (Element: sig type t end) :
 S with type element = Element.t
    and type 'a stream = Element.t Stream.t -> 'a
    and type 'a return = 'a =
struct
  type 'a return = 'a
  type element = Element.t
  type 'a stream = Element.t Stream.t -> 'a
  type context = { stream: Element.t Stream.t code }

  let init f = .< fun stream -> .~(f { stream = .<stream>. }) >.
  type 'a mkcall = { mkcall : 'b. context -> (context -> 'a code -> 'b return code) -> 'b return code }

  let genfun locus mkbody =
    ignore @@ Genletrec.genletrec locus
      (fun f ->
        .<fun stream ->
          .~(let context = {stream = .<stream>.} in
             mkbody context { mkcall = fun context h ->
             .< let x = .~f .~(context.stream) in
                .~(h context .<x>.) >.})>.)

  let return _ v = v

  let junk s k = .< (Stream.junk .~(s.stream); .~(k s)) >.

  let peek ctxt k =
    .< match Stream.peek .~(ctxt.stream) with
      | None -> .~(k ctxt None)
      | Some x -> .~(k ctxt (Some .<x>.)) >.
end

module Channelcode :
 S with type element = char
    and type 'a stream = in_channel -> 'a
    and type 'a return = 'a =
struct
  type 'a return = 'a
  type element = char
  type 'a stream = in_channel -> 'a
  type context = { channel: in_channel code }

  type 'a mkcall = { mkcall : 'b. context -> (context -> 'a code -> 'b return code) -> 'b return code }

  let genfun locus mkbody =
    ignore @@ Genletrec.genletrec locus
      (fun f ->
        .<fun channel ->
          .~(let context = {channel = .<channel>.} in
             mkbody context { mkcall = fun context h ->
             .< let x = .~f .~(context.channel) in
                .~(h context .<x>.) >.})>.)

  let return _ v = v

  let junk s k = .< (Asp_utilities.Unstaged.junk_channel .~(s.channel); .~(k s)) >.

  let peek ctxt k =
    .< match Asp_utilities.Unstaged.peek_channel .~(ctxt.channel) with
      | None -> .~(k ctxt None)
      | Some x -> .~(k ctxt (Some .<x>.)) >.

  let init f = .< fun ch -> .~(f  { channel = .< ch >. }) >.
end

module type ARRAY_CODE = sig
  type elt
  type t
  val unsafe_get : (t -> int -> elt) code
  val length : (t -> int) code
end

module String_code : ARRAY_CODE with type elt = char and type t = string =
struct
  type elt = char
  type t = string
  let unsafe_get = .<String.unsafe_get>.
  let length = .<String.length>.
end

module Bigstring_code : ARRAY_CODE with type elt = char and type t = (char, char, Bigarray.c_layout) Bigarray.Array1.t =
struct
  type elt = char
  type t = (char, char, Bigarray.c_layout) Bigarray.Array1.t
  let unsafe_get = .<Bigarray.Array1.unsafe_get>.
  let length = .<Bigarray.Array1.dim>.
end

module Array_code (E: sig type t end) : ARRAY_CODE with type elt = E.t and type t = E.t array =
struct
  type elt = E.t
  type t = E.t array
  let unsafe_get = .<Array.unsafe_get>.
  let length = .<Array.length>.
end

module Arraylikecode (A: ARRAY_CODE) :
 S with type 'a stream = index:int -> A.t -> 'a * int
    and type element = A.elt
    and type 'a return = 'a * int =
struct
  type element = A.elt
  type 'a stream = index:int -> A.t -> 'a * int
  type 'a return = 'a * int
  type context = {
      index: (int * int code);
      string: A.t code;
      length: int code;
    }

  let index context = match context.index with (0,i) -> i | (n,i) -> .< n + .~i >.

  type 'a mkcall = { mkcall : 'b. (context -> (context -> 'a code -> 'b code) -> 'b code)}

  let genfun locus mkbody  =
    ignore @@ Genletrec.genletrec locus
      (fun f ->
        .< fun i n s ->
          .~(let context = { index = (0, .<i>.);
                             string = .<s>.;
                             length = .<n>. } in
             mkbody context { mkcall = fun context h ->
             .< let (x, j) = .~f .~(index context) .~(context.length) .~(context.string) in
                 .~(h {context with index = (0,.<j>.) } .<x>.) >.}) >.)

  let return context v =
    .< (.~v, .~(index context)) >.

  let junk ({ index = (s,d); _ } as context) k =
    k { context with index = (s+1,d) }

  let peek context f =
    let i = index context in
    .< if .~i >= .~(context.length) then .~(f context None)
      else let c = .~(A.unsafe_get) .~(context.string) .~i in
           .~(f context (Some .<c>.)) >.

  let init f =
    .< fun ~index s ->
       let n = .~(A.length) s in
       .~(let context = {
              index = (0, .<index>.);
              string = .<s>.;
              length = .<n>.;
            } in
          f context) >.
end

module Stringcode = Arraylikecode (String_code)
module Bigstringcode = Arraylikecode (Bigstring_code)
module Arraycode(E: sig type t end) = Arraylikecode (Array_code(E))
