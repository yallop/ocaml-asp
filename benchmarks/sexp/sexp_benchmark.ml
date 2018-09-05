(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let filenames = [
    (262144, "../../../benchmarks/sexp/data/sexp.262144");
    (524288, "../../../benchmarks/sexp/data/sexp.524288");
    (786432, "../../../benchmarks/sexp/data/sexp.786432");
    (1048576, "../../../benchmarks/sexp/data/sexp.1048576");
    (1310720, "../../../benchmarks/sexp/data/sexp.1310720");
    (1572864, "../../../benchmarks/sexp/data/sexp.1572864");
    (1835008, "../../../benchmarks/sexp/data/sexp.1835008");
    (2097152, "../../../benchmarks/sexp/data/sexp.2097152");
  ]
let args = List.map fst filenames
let files = List.map (fun (size, filename) -> (size, Core.In_channel.read_all filename)) filenames

let ocamlyacc_sexp n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> ignore (Sexp_parser.sexp Sexp_lexer.token (Lexing.from_string file)))

let staged_sexp n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Sexp_staged_combinator_parser.Parser.staged_complete file)

let unstaged_sexp n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Sexp_unstaged_combinator_parser.Parser.unstaged_complete file)

open Core
open Core_bench.Std

let () =
  Command.run (Bench.make_command [
    Bench.Test.create_indexed ~name:"ocamlyacc_sexp" ~args
      ocamlyacc_sexp;

    Bench.Test.create_indexed ~name:"staged_sexp" ~args
      staged_sexp;

    Bench.Test.create_indexed ~name:"unstaged_sexp" ~args
      unstaged_sexp;
  ])
