(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module CharSet = Set.Make(Char)
type interval = char * char

module Mk =
struct
  open Parsetree

  let pattern : pattern_desc -> pattern =
    fun ppat_desc ->
    { ppat_desc; ppat_loc = Location.none; ppat_attributes = [] }

  let expression : expression_desc -> expression =
    fun pexp_desc ->
    { pexp_desc; pexp_loc = Location.none; pexp_attributes = [] }

  let interval : char -> char -> pattern =
    fun l h -> if l <> h then pattern @@ Ppat_interval (Pconst_char l, Pconst_char h)
               else pattern @@ Ppat_constant (Pconst_char l)

  let intervals : interval list -> pattern = function
    | [] -> failwith "empty interval list not supported"
    | (l,h)::is ->
       List.fold_left (fun p (l,h) -> pattern @@ Ppat_or (interval l h, p))
         (interval l h) is

  let case : 'a. pattern -> 'a code -> (char -> 'a) Print_code.pat_code =
    fun pat code ->
    let fv, rhs = Obj.magic code in
    (Obj.magic (fv, expression (Pexp_fun (Asttypes.Nolabel, None, pat, rhs))))
end

let intervals : CharSet.t -> interval list =
  let add c = function
    | (l,h) :: tail when Char.code c = succ (Char.code h) -> (l, c) :: tail
    | l -> (c, c) :: l
  in fun c -> List.rev (CharSet.fold add c [])

let ipat c = Mk.intervals (intervals c)


let ifmem : 'a. char code -> (Set.Make(Char).t * 'a code) list -> otherwise:'a code -> 'a code =
  fun c cases ~otherwise ->
  (* let all_sets = List.fold_right (fun (s1,_) s2 ->
   *                    let s3 = CharSet.union s1 s2 in
   *                    (\* check disjointness *\)
   *                    assert (CharSet.cardinal s1 + CharSet.cardinal s2 = CharSet.cardinal s3);
   *                    s3) cases CharSet.empty in
   * (\* check coverage *\)
   * assert (CharSet.cardinal all_sets = 255); *)
 Print_code.make_match c
 @@ (List.map (fun (lhs, rhs) -> Mk.case (ipat lhs) rhs) cases
     @ [.< fun _ -> .~otherwise >. [@metaocaml.functionliteral] ])
