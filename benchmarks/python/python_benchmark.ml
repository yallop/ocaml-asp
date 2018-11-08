(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let filenames =
  let files = Array.to_list @@ Sys.readdir "../../../benchmarks/python/data" in
  let pyfiles = List.filter (fun x -> Filename.extension x <> ".py") files in
  [(1, pyfiles)]

let args = List.map fst filenames
let files = ListLabels.map filenames
              ~f:(fun (n, names) -> (n, List.map Core.In_channel.read_all names))

let unstaged_python n =
  let fs = List.assoc n files in
  Core.Staged.stage @@ fun () ->
  let module P = Python_unstaged_combinator_parser in
  ListLabels.iter fs
    ~f:(fun _ -> assert false )

open Core
open Core_bench.Std

let () =
  Command.run (Bench.make_command [
    Bench.Test.create_indexed ~name:"unstaged_python" ~args
      unstaged_python;
  ])

