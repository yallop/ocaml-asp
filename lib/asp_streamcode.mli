(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Asp_types

module type S = STREAMCODE

module Stringcode : S
       with type element = char
        and type 'a stream = index:int -> string -> 'a * int
        and type 'a return = 'a * int

module Arraycode (E: sig type t end) : S
       with type element = E.t
        and type 'a stream = index:int -> E.t array -> 'a * int
        and type 'a return = 'a * int

module Bigstringcode : S
       with type element = char
        and type 'a stream = index:int -> (char, char, Bigarray.c_layout) Bigarray.Array1.t -> 'a * int
        and type 'a return = 'a * int

module Channelcode : S
       with type element = char
        and type 'a stream = in_channel -> 'a
        and type 'a return = 'a

module Streamcode (E: sig type t end) : S
       with type element = E.t
        and type 'a stream = E.t Stream.t -> 'a
        and type 'a return = 'a
