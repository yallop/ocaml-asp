(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module Unstaged :
sig
  val toString : char list -> string
  val toInt : char list -> int

  val peek_channel : in_channel -> char option
  val junk_channel : in_channel -> unit
end

type _ chr = Chr : char -> char chr [@@unboxed]

module Char_tag : Asp.Types.TAG with type 'a t = 'a chr

module Char_element : Asp.Types.TOKEN with type 'a tag = 'a chr
                                       and type t = char
