/*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

%{
%}

/* Tokens */

%token LBRACKET RBRACKET LBRACE RBRACE COMMA COLON
%token NULL TRUE FALSE
%token <string> STRING DECIMAL
%token EOF

%start values
%type <int> values
%%

values: /**/                              { 0 }
| value values                            { $1 + $2 }
;

value: obj                                { $1 }
|      arr                                { $1 }
|      STRING                             { 1 }
|      DECIMAL                            { 1 }
|      NULL                               { 1 }
|      TRUE                               { 1 }
|      FALSE                              { 1 }
;

obj: LBRACE member_list RBRACE            { $2 }
;

arr: LBRACKET value_list RBRACKET         { $2 }
;

member_list: /* */                        { 0 }
| member                                  { $1 }
| member COMMA member_list                { $1 + $3 }
;

value_list: /* */                         { 0 }
| value                                   { $1 }
| value COMMA value_list                  { $1 + $3 }
;

member: STRING                            { 1 }
|       STRING COLON value                { $3 }
;
%%
