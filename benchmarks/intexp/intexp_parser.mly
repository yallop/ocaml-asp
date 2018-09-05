/*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

%{
%}

/* Tokens */

%token LET IN IF THEN ELSE LPAREN RPAREN PLUS MINUS TIMES EQUAL
%token <int> INT
%token <string> ID
%token EOF

%start exp
%type <(string * int) list -> int> exp
%%

exp: eqexp  { $1 }
|    letexp { $1 }
|    ifexp  { $1 }
;

eqexp: plusexp                  { $1 }
|      eqexp EQUAL plusexp      { fun env -> if $1 env = $3 env then 1 else 0 }
;

plusexp: timesexp               { $1 }
|        plusexp PLUS timesexp  { fun env -> $1 env + $3 env }
|        plusexp MINUS timesexp { fun env -> $1 env - $3 env }
;

timesexp: atom                  { $1 }
|         timesexp TIMES atom   { fun env -> $1 env * $3 env }
;

atom: LPAREN exp RPAREN         { $2 }
|     ID                        { List.assoc $1 }
|     INT                       { fun _ -> $1 }
;

letexp: LET ID EQUAL exp IN exp { fun env -> $6 (($2, $4 env) :: env) }
;

ifexp: IF exp THEN exp ELSE exp { fun env -> if $2 env <> 0 then $4 env else $6 env }
;

%%
