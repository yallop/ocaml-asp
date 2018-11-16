(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

include Asp.Types.TOKEN
        with type t = Json_tokens_base.t
        with type utag = Json_tokens_base.utag
        with type 'a tag = 'a Json_tokens_base.tag
