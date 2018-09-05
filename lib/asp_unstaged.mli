(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module Parse (Tag: Asp_types.TAG) :
sig
  type 'a v = 'a
  include Asp_types.PARSE with type 'a v := 'a v
                           and type 'a tag := 'a Tag.t

  type 'a type_checked
  val type_check : 'a t -> 'a type_checked
  (** [type_check p] checks that a parser satisfies the typing constraints
      -- e.g. that the grammar is unambiguous and does not contain left-recursion *)

  type token = Tok : 'a Tag.t * 'a -> token

  val parse : 'a type_checked -> token Stream.t -> 'a
  (** [parse p s] parses the stream of input tokens [s] using the parser [p]
      and returns the result, or fails with an exception. *)
end
