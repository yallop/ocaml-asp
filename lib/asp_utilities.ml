(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module Unstaged =
struct
  let toString cs =
    (* TODO: make this faster *)
    let n = List.length cs in
    let s = Bytes.make n '\000' in
    let rec loop i = function
      | [] -> s
      | c :: cs -> Bytes.unsafe_set s i c; loop (i+1) cs
      in
      Bytes.unsafe_to_string (loop 0 cs)

  let toInt : char list -> int =
    let rec loop l acc = match l with
      | '0' :: l -> loop l (acc * 10)
      | '1' :: l -> loop l (acc * 10 + 1)
      | '2' :: l -> loop l (acc * 10 + 2)
      | '3' :: l -> loop l (acc * 10 + 3)
      | '4' :: l -> loop l (acc * 10 + 4)
      | '5' :: l -> loop l (acc * 10 + 5)
      | '6' :: l -> loop l (acc * 10 + 6)
      | '7' :: l -> loop l (acc * 10 + 7)
      | '8' :: l -> loop l (acc * 10 + 8)
      | '9' :: l -> loop l (acc * 10 + 9)
      | [] -> acc
      | _ -> assert false
    in fun l -> loop l 0

  let peek_channel : in_channel -> char option =
    fun ch ->
    let pos = pos_in ch in
    match input_char ch with
    | exception End_of_file -> None
    | c -> seek_in ch pos; Some c

  let junk_channel : in_channel -> unit =
    fun ch -> seek_in ch (succ (pos_in ch))
end

type _ chr = Chr : char -> char chr [@@unboxed]

module Char_tag =
struct
  type 'a t = 'a chr

  let compare : type a b. a t -> b t -> (a,b) Asp.Types.cmp =
    fun (Chr c1) (Chr c2) ->
    match () with
    | _ when c1 < c2 -> Leq
    | _ when c2 < c1 -> Geq
    | _              -> Eql

  let print : type a. Format.formatter -> a t -> unit =
    fun fmt (Chr c) -> Format.fprintf fmt "%c " c
end

module Char_element =
struct
  type t = char
  type utag = char
  let pp_utag = Format.pp_print_char
  module Ord = Char
  module CharSet = Set.Make(Char)
  let all =
    let rec loop i s =
      if i > 255 then s
      else loop (succ i) (CharSet.add (Char.chr i) s)
    in loop 0 CharSet.empty
  let to_int = Char.code
  let within_ (l:char) (r:char) (c: char code) =
    assert (r >= l);
    if l = r then  .< l = .~c>.
    else .<l <= .~c && .~c <= r>.

  type 'a tag = 'a chr
  let inj : type a. a tag -> char = fun (Chr c) -> c

  let char_of_tag : type a.a tag -> char = function (Chr c) -> c

  let test_tag : type a b.complete:bool -> a tag list -> t code -> (a code option -> b code) -> b code =
    fun ~complete cs c' k ->
    if complete then k (Some (Obj.magic c')) else
    Asp_char_patmatch.ifmem c'
      [CharSet.of_list (List.map char_of_tag cs), k (Some (Obj.magic c'))]
      ~otherwise:(k None)
end
