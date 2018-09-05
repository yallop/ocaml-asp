(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Asp.Utilities.Unstaged

module Charparsing :
sig
  val chr        : char   -> char Asp.Staged.Parse(Char_element).t
  val charset    : string -> char Asp.Staged.Parse(Char_element).t
  val complement : string -> char Asp.Staged.Parse(Char_element).t
  val lower                : char Asp.Staged.Parse(Char_element).t
  val upper                : char Asp.Staged.Parse(Char_element).t
  val digit                : char Asp.Staged.Parse(Char_element).t
  val sym                  : char Asp.Staged.Parse(Char_element).t
  val whitespace           : char Asp.Staged.Parse(Char_element).t
  val int                  : int  Asp.Staged.Parse(Char_element).t
  val string               : string Asp.Staged.Parse(Char_element).t
end

module Tokparsing(Token: Asp.Types.TOKEN) :
sig
  val plus : 'a      Asp.Staged.Parse(Token).t ->
             'a list Asp.Staged.Parse(Token).t
end

