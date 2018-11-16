(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let filenames = [
    1, "../../../benchmarks/json/data/all.json"
  ]
let args = List.map fst filenames
let files = List.map (fun (size, filename) -> (size, Core.In_channel.read_all filename)) filenames

let ocamlyacc_json n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> ignore (Json_parser.values Json_lexer.token (Lexing.from_string file)))

let staged_json n =
  let file =  List.assoc n files in
  Core.Staged.stage (fun () -> Json_staged_combinator_parser.Parser.staged_complete file)

let unstaged_json n =
  let file = List.assoc n files in
  Core.Staged.stage (fun () -> Json_unstaged_combinator_parser.Parser.unstaged_complete file)

open Core
open Core_bench.Std


let () =
  Command.run (Bench.make_command [
    Bench.Test.create_indexed ~name:"ocamlyacc_json" ~args
      ocamlyacc_json;
    
    Bench.Test.create_indexed ~name:"staged_json" ~args
      staged_json;

    Bench.Test.create_indexed ~name:"unstaged_json" ~args
      unstaged_json;
  ])
