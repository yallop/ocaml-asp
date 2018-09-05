/*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

%{
%}

/* Tokens */

%token LBRACKET RBRACKET MINUS SLASH STAR CASTLE
%token <string> TAG STRING COORDINATE INT INTDOT
%token EOF

%start games
%type <[`DRAWN | `OTHER | `WON] list> games
%%

games: /* */                              { [];  }
|      game games                         { $1 :: $2 }
;

game:  metadata moves result              { $3 }
;

metadata: /* */                           { [] }
|         metadatum metadata              { $1 :: $2 }
;

metadatum: LBRACKET TAG STRING RBRACKET   { ($2, $3) }
;

moves: /* */                              { [] }
|     move moves                          { $1 :: $2 }
;

move: INTDOT coordinate coordinate        { ($2, Some $3) }
|     INTDOT coordinate                   { ($2, None) }

;

coordinate: COORDINATE                    { `COORD $1 }
|           CASTLE                        { `CASTLE }
    
result: INT MINUS INT                     { `WON   }
|       INT SLASH INT MINUS INT SLASH INT { `DRAWN }
|       STAR                              { `OTHER }
%%
