(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

{
open Ppm_parser

}

rule token = parse
  | 'P' '3'                      { P3 }
  | ['0'-'9']+                   { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '#' [^'\n']*                 { (* comment *) token lexbuf }
  | '\n'                         { Lexing.new_line lexbuf; token lexbuf } 
  | ['\r' '\n' ' ' '\t' ]
                                 { token lexbuf }
  | _  as c                      { Printf.ksprintf failwith "Unexpected: %c" c }
  | eof                          { EOF }

{
}

