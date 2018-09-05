(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let filenames = [
    (32, "../../../benchmarks/pgn/data/1610-1899_32k.pgn");
    (64, "../../../benchmarks/pgn/data/1610-1899_64k.pgn");
    (125, "../../../benchmarks/pgn/data/1610-1899_125k.pgn");
    (250, "../../../benchmarks/pgn/data/1610-1899_250k.pgn");
    (500, "../../../benchmarks/pgn/data/1610-1899_500k.pgn");
    (1024, "../../../benchmarks/pgn/data/1610-1899_1MB.pgn");
    (2048, "../../../benchmarks/pgn/data/1610-1899_2MB.pgn");
    (4160, "../../../benchmarks/pgn/data/1610-1899.pgn");
  ]
let args = List.map fst filenames
let files = List.map (fun (size, filename) -> (size, Core.In_channel.read_all filename)) filenames

let ocamlyacc_pgn n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> ignore (Pgn_parser.games Pgn_lexer.token (Lexing.from_string file)))

let staged_pgn n =
  let file =  List.assoc n files in
  Core.Staged.stage (fun () -> Pgn_staged_combinator_parser.Parser.staged_complete file)

let unstaged_pgn n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Pgn_unstaged_combinator_parser.Parser.unstaged_complete file)

open Core
open Core_bench.Std


let () =
  Command.run (Bench.make_command [
    Bench.Test.create_indexed ~name:"ocamlyacc_pgn" ~args
      ocamlyacc_pgn;

    Bench.Test.create_indexed ~name:"staged_pgn" ~args
      staged_pgn;

    Bench.Test.create_indexed ~name:"unstaged_pgn" ~args
      unstaged_pgn;
  ])
