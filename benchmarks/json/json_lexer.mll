(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

{
open Json_parser

}

rule token = parse
  | '['                                 { LBRACKET }
  | ']'                                 { RBRACKET }
  | '{'                                 { LBRACE }
  | '}'                                 { RBRACE }
  | ','                                 { COMMA }
  | ':'                                 { COLON }
  | "null"                              { NULL }
  | "true"                              { TRUE }
  | "false"                             { FALSE }
  | '"' ([^'"'] | ('\\' _))* '"' as s   { STRING s }
  | ['0'-'9']+ ('.' ['0'-'9']+)? as s   { DECIMAL s }
  | [' ' '\t' '\r' '\n']                { token lexbuf }
  | eof                                 { EOF }
  | _ as c                              { Printf.kprintf failwith "Unexpected character %c" c }
{
}

