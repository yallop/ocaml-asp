(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

include sig (* TODO: remove; use Asp.Types.TOKEN *)
  type t
  type utag
  module Ord : Set.OrderedType with type t = utag

  val pp_utag : Format.formatter -> utag -> unit

  val to_int : utag -> int
  val all : Set.Make(Ord).t
  val within_ : utag -> utag -> t code -> bool code

  type _ tag
  val inj : _ tag -> utag
  val test_tag : complete:bool -> 'a tag list -> t code -> ('a code option -> 'b code) -> 'b code
end     with type t = Python_tokens_base.t
        with type utag = Python_tokens_base.utag
        with type 'a tag = 'a Python_tokens_base.tag
