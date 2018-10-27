(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Asp.Utilities.Unstaged

module Charparsing =
struct
  module P = Asp.Staged.Parse(Char_element)
  open P

  let chr c = tok (Chr c)

  let charset s =
    let len = String.length s in
    let rec loop i acc =
      if i < len then
        loop (i+1) (acc <|> chr s.[i])
      else
        acc
    in loop 0 bot

  let complement_string s' = (* assumes characters in s' are distinct *)
    let s = Bytes.make (256 - String.length s') '\000' in
    let j = ref 0 in
    for i = 0 to 255 do
      let c = Char.chr i in
      if not (String.contains s' c) then begin
          Bytes.set s (!j) c;
          incr j
        end
    done;
    Bytes.unsafe_to_string s

  let complement s = charset (complement_string s)

  let lower = charset "abcdefghijklmnopqrstuvwxyz"
  let upper = charset "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  let digit = charset "0123456789"
  let sym = charset "_'?"
  let whitespace = charset " \t\r\n"

  let int =
    (digit >>> star digit)
    $ fun s -> .< Asp.Utilities.Unstaged.Unstaged.toInt (fst .~s :: snd .~s) >.

  let string =
    let open Unstaged in
    (chr '"' >>> star (complement "\"") >>> chr '"')
    $ fun s -> .< toString (snd (fst .~s)) >.
end

module Tokparsing(Token: Asp.Types.TOKEN) =
struct
  module P = Asp.Staged.Parse(Token)
  open P
  let plus g = (g >>> star g) $ fun p -> .<fst .~p :: snd .~p>.
end

