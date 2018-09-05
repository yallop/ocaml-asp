(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module Ir(E: Asp_types.TOKEN):
sig
  type 'a t

  val return : 'a code -> 'a t
  val (>>=) : 'a t -> ('a code -> 'b t) -> 'b t
  val fail : string -> 'a t

  val junk : unit t
  val peek_mem : Set.Make(E.Ord).t -> ([`Yes | `No | `Eof] -> 'b t) -> 'b t
  val peek : 'a E.tag list -> ([`Yes of 'a code | `No | `Eof] -> 'b t) -> 'b t

  val fix : ('b t -> 'b t) -> 'b t

  module Codegen (S: Asp_types.STREAMCODE with type element = E.t) :
  sig
    val generate : 'a t -> 'a S.stream code
  end
end

