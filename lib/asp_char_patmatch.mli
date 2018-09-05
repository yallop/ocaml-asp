(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

val ifmem : char code -> (Set.Make(Char).t * 'a code) list -> otherwise:'a code -> 'a code
