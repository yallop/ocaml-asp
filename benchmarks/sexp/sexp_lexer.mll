(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

{
open Sexp_parser
}

rule token = parse
  | ['A'-'Z' 'a'-'z']+ ['0'-'9' 'a'-'z' 'A'-'Z']* { ATOM (Lexing.lexeme lexbuf) }
  | ['\r' '\n' ' ' '\t' ]                         { token lexbuf }
  | '('                                           { LPAREN }
  | ')'                                           { RPAREN }
  | eof                                           { EOF }

{

}
