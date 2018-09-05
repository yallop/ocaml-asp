(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let filenames = [
    (262144, ("../../../benchmarks/intexp/data/intexp.262144", 2491880767779407440));
    (524288, ("../../../benchmarks/intexp/data/intexp.524288", -4445224671334544474));
    (786432, ("../../../benchmarks/intexp/data/intexp.786432", -850148006049275809));
    (1048576, ("../../../benchmarks/intexp/data/intexp.1048576", 0));
    (1310720, ("../../../benchmarks/intexp/data/intexp.1310720", 660851688233856212));
    (1572864, ("../../../benchmarks/intexp/data/intexp.1572864", 0));
    (1835008, ("../../../benchmarks/intexp/data/intexp.1835008", 1));
    (2097152, ("../../../benchmarks/intexp/data/intexp.2097152", 2775427195430798160));
  ]
let args = List.map fst filenames
let files = List.map (fun (size, (filename, expected)) -> (size, (Core.In_channel.read_all filename, expected))) filenames

let ocamlyacc_intexp n =
  let exp, expected =  List.assoc n files in
  Core.Staged.stage (fun () ->
      let result = Intexp_parser.exp Intexp_lexer.token (Lexing.from_string exp)
      in assert (result [] = expected))

let staged_intexp n =
  let exp, expected =  List.assoc n files in
  Core.Staged.stage (fun () ->
      let result = Intexp_staged_combinator_parser.Parser.staged_complete exp
      in assert (result [] = expected))

let unstaged_intexp n =
  let exp, expected =  List.assoc n files in
  Core.Staged.stage (fun () ->
      let result = Intexp_unstaged_combinator_parser.Parser.unstaged_complete exp
      in assert (result [] = expected))

open Core
open Core_bench.Std

let () =
  Command.run (Bench.make_command [
    Bench.Test.create_indexed ~name:"ocamlyacc_intexp" ~args
      ocamlyacc_intexp;

    Bench.Test.create_indexed ~name:"staged_intexp" ~args
      staged_intexp;

    Bench.Test.create_indexed ~name:"unstaged_intexp" ~args
      unstaged_intexp;
  ])
