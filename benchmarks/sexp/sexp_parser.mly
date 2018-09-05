/*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

%{
%}

/* Tokens */

%token LPAREN RPAREN
%token<string> ATOM
%token EOF

%start sexp
%type <int> sexp
%%

sexp:    ATOM { 1 }
|        LPAREN sexps RPAREN  { $2 }
;

sexps:   /* */      { 0 }
|        sexp sexps { $1 + $2 }
;

%%
