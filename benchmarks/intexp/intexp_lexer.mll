(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

{
open Intexp_parser

let keywords = 
  ["let" , LET;
   "in"  , IN;
   "if"  , IF;
   "then", THEN;
   "else", ELSE]
}

let lower = ['a'-'z']
let upper = ['A'-'Z']
let digit = ['0'-'9']
let sym   = ['_' '?']

rule token = parse
  | lower (upper | lower | digit | sym)*   { let s = Lexing.lexeme lexbuf in
                                             try List.assoc s keywords
                                             with Not_found -> ID s }
  | '('                                    { LPAREN }
  | ')'                                    { RPAREN }
  | '+'                                    { PLUS }
  | '-'                                    { MINUS }
  | '*'                                    { TIMES }
  | '='                                    { EQUAL }
  | digit+                                 { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '\n'                                   { Lexing.new_line lexbuf; token lexbuf } 
  | ['\r' '\n' ' ' '\t' ]                  { token lexbuf }
  | _  as c                                { Printf.ksprintf failwith "Unexpected: %c" c }
  | eof                                    { EOF }
{
}

