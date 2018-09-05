(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module Parse (Token: Asp_types.TOKEN) :
sig
  include Asp_types.PARSE with type 'a v := 'a code
                           and type 'a tag := 'a Token.tag

  type 'a type_checked
  val type_check : 'a t -> 'a type_checked
  (** [type_check p] checks that a parser satisfies the typing constraints
      -- e.g. that the grammar is unambiguous and does not contain left-recursion *)

  module Parser (S: Asp_types.STREAMCODE with type element = Token.t) :
  sig
    val compile : 'a type_checked -> 'a S.stream code
    (** [compile p] generates code that parses the stream of input tokens [s]
        using the parser [p] and returns the result, or fails with an exception. *)
  end
end
