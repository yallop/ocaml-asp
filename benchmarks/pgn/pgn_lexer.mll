(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

{
open Pgn_parser

}

rule token = parse
  | '['                          { LBRACKET }
  | ']'                          { RBRACKET }
  | '/'                          { SLASH }
  | '-'                          { MINUS }
  | '*'                          { STAR }
  | '"' ([^'"']*) '"'            { STRING (Lexing.lexeme lexbuf) }
  (* | ['K' 'Q' 'R' 'B' 'N' 'a'-'h'] ?
   *   'x'?
   *   ['a'-'h']
   *   ['1'-'8']
   *   ('=' ['K' 'Q' 'R' 'B' 'N']) ?
   *   ['#' '+'] ?                  { pr "coord\n"; COORDINATE (Lexing.lexeme lexbuf) } *)
  | ['A'-'Z' 'a'-'z']+ ['0'-'9'] ['a'-'z' 'A'-'Z' '0'-'9' '=' '#' '-' '+']*
                                 { COORDINATE (Lexing.lexeme lexbuf) } 
  | 'O' '-' 'O'
       ('-' 'O')?
       ['#' '+']?                { CASTLE }
  | ['0'-'9']+  '.'              { INTDOT (Lexing.lexeme lexbuf) }
  | ['0'-'9']+                   { INT (Lexing.lexeme lexbuf) }
  | ['A'-'Z'] ['a'-'z' 'A'-'Z']*
                                 { TAG (Lexing.lexeme lexbuf) }
  | '\n'                         { Lexing.new_line lexbuf; token lexbuf } 
  | ['\r' '\n' ' ' '\t' ]
                                 { token lexbuf }
  | _  as c                      { Printf.ksprintf failwith "Unexpected: %c" c }
  | eof                          { EOF }

{
}

