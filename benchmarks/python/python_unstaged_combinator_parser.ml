module P1 = Asp.Unstaged.Parse(Asp.Utilities.Unstaged.Char_tag)
module P2 = Asp.Unstaged.Parse(Python_tokens_base.Tag)

module Tok = Python_tokens_base
module F = Fixes

module Parser =
struct
  open P2

  type ('a, 'b) either = Left of 'a | Right of 'b
  let left x = Left x and right x = Right x
  let plus x = x >>> star x $ fun (x,xs) -> List.cons x xs
  let (<+>) l r = (l $ left) <|> (r $ right) (* temporary *)

  let break_stmt = tok Tok.KW_break
  let continue_stmt = tok Tok.KW_continue
  let vfpdef = tok Tok.NAME
  let pass_stmt = tok Tok.KW_pass
  let dotted_name = tok Tok.NAME >>> star (tok Tok.DOT >>> tok Tok.NAME)
  let global_stmt = tok Tok.KW_global >>> tok Tok.NAME >>>
                      star (tok Tok.COMMA >>> tok Tok.NAME)
  let nonlocal_stmt = tok Tok.KW_nonlocal >>> tok Tok.NAME >>>
                      star (tok Tok.COMMA >>> tok Tok.NAME)
  let encoding_decl = tok Tok.NAME

  let dotted_as_name = dotted_name >>> maybe (tok Tok.KW_as >>> tok Tok.NAME)
  let dotted_as_names =  dotted_as_name >>> star (tok Tok.COMMA >>> dotted_as_name)
  let import_name = tok Tok.KW_import >>> dotted_as_names

  let import_as_name = tok Tok.NAME >>> maybe (tok Tok.KW_as >>> tok Tok.NAME)

  let import_as_names = import_as_name >>> star (tok Tok.COMMA >>> import_as_name) >>>
                          maybe (tok Tok.COMMA)

  let import_from = tok Tok.KW_from >>>
                    ((star (tok Tok.DOT <+> tok Tok.ELLIPSIS) >>> dotted_name)
                   <+> plus (tok Tok.DOT <+> tok Tok.ELLIPSIS)) >>>
                    tok Tok.KW_import >>>
                    (tok Tok.STAR
                  <+> (tok Tok.LPAR >>> import_as_names >>> tok Tok.RPAR)
                  <+> import_as_names)

  let import_stmt = import_name <+> import_from

  let augassign =
    any (List.map tok Tok.[
             PLUSEQUAL; MINEQUAL; STAREQUAL; ATEQUAL; SLASHEQUAL; PERCENTEQUAL;
             AMPEREQUAL; VBAREQUAL; CIRCUMFLEXEQUAL; LEFTSHIFTEQUAL;
             RIGHTSHIFTEQUAL; DOUBLESTAREQUAL; DOUBLESTAREQUAL
      ])

  let comp_op =
    any (List.map tok Tok.[
          LESS; GREATER; EQEQUAL; GREATEREQUAL; LESSEQUAL; NOTEQUAL;
          KW_in;
         ])
      <+> (tok Tok.KW_not >>> tok Tok.KW_in)
      <+> tok Tok.KW_is
      <+> (tok Tok.KW_is >>> tok Tok.KW_not)


  module Python_index =
  struct
    open Fixes

    type _ t =
      Stmt : unit t
    let equalp : type a b. a t -> b t -> (a, b) eql option =
      fun Stmt Stmt -> Some Refl
  end

  module Python_fix = Fixes.Fix(Python_index)

  (*
  # Grammar for Python

  # NOTE WELL: You should also follow all the steps listed at
  # https://devguide.python.org/grammar/

  # Start symbols for the grammar:
  #       single_input is a single interactive statement;
  #       file_input is a module or sequence of commands read from an input file;
  #       eval_input is the input for the eval() functions.
  # NB: compound_stmt in single_input is followed by extra NEWLINE!
  single_input: NEWLINE | simple_stmt | compound_stmt NEWLINE
  file_input: (NEWLINE | stmt)* ENDMARKER
  eval_input: testlist NEWLINE* ENDMARKER

  decorator: '@' dotted_name [ '(' [arglist] ')' ] NEWLINE
  decorators: decorator+
  decorated: decorators (classdef | funcdef | async_funcdef)

  async_funcdef: 'async' funcdef
  funcdef: 'def' NAME parameters ['->' test] ':' suite

  parameters: '(' [typedargslist] ')'
  typedargslist: (tfpdef ['=' test] (',' tfpdef ['=' test])* [',' [
          '*' [tfpdef] (',' tfpdef ['=' test])* [',' ['**' tfpdef [',']]]
        | '**' tfpdef [',']]]
    | '*' [tfpdef] (',' tfpdef ['=' test])* [',' ['**' tfpdef [',']]]
    | '**' tfpdef [','])
  tfpdef: NAME [':' test]
  varargslist: (vfpdef ['=' test] (',' vfpdef ['=' test])* [',' [
          '*' [vfpdef] (',' vfpdef ['=' test])* [',' ['**' vfpdef [',']]]
        | '**' vfpdef [',']]]
    | '*' [vfpdef] (',' vfpdef ['=' test])* [',' ['**' vfpdef [',']]]
    | '**' vfpdef [',']
  )

  stmt: simple_stmt | compound_stmt
  simple_stmt: small_stmt (';' small_stmt)* [';'] NEWLINE
  small_stmt: (expr_stmt | del_stmt | pass_stmt | flow_stmt |
               import_stmt | global_stmt | nonlocal_stmt | assert_stmt)
  expr_stmt: testlist_star_expr (annassign | augassign (yield_expr|testlist) |
                       ('=' (yield_expr|testlist_star_expr))* )
  annassign: ':' test ['=' test]
  testlist_star_expr: (test|star_expr) (',' (test|star_expr))* [',']
  # For normal and annotated assignments, additional restrictions enforced by the interpreter
  del_stmt: 'del' exprlist
  flow_stmt: break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt
  return_stmt: 'return' [testlist]
  yield_stmt: yield_expr
  raise_stmt: 'raise' [test ['from' test]]

  assert_stmt: 'assert' test [',' test]

  compound_stmt: if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | funcdef | classdef | decorated | async_stmt
  async_stmt: 'async' (funcdef | with_stmt | for_stmt)
  if_stmt: 'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]
  while_stmt: 'while' test ':' suite ['else' ':' suite]
  for_stmt: 'for' exprlist 'in' testlist ':' suite ['else' ':' suite]
  try_stmt: ('try' ':' suite
             ((except_clause ':' suite)+
              ['else' ':' suite]
              ['finally' ':' suite] |
             'finally' ':' suite))
  with_stmt: 'with' with_item (',' with_item)*  ':' suite
  with_item: test ['as' expr]
  # NB compile.c makes sure that the default except clause is last
  except_clause: 'except' [test ['as' NAME]]
  suite: simple_stmt | NEWLINE INDENT stmt+ DEDENT

  test: or_test ['if' or_test 'else' test] | lambdef
  test_nocond: or_test | lambdef_nocond
  lambdef: 'lambda' [varargslist] ':' test
  lambdef_nocond: 'lambda' [varargslist] ':' test_nocond
  or_test: and_test ('or' and_test)*
  and_test: not_test ('and' not_test)*
  not_test: 'not' not_test | comparison
  comparison: expr (comp_op expr)*
  # <> isn't actually a valid comparison operator in Python. It's here for the
  # sake of a __future__ import described in PEP 401 (which really works :-)

  star_expr: '*' expr
  expr: xor_expr ('|' xor_expr)*
  xor_expr: and_expr ('^' and_expr)*
  and_expr: shift_expr ('&' shift_expr)*
  shift_expr: arith_expr (('<<'|'>>') arith_expr)*
  arith_expr: term (('+'|'-') term)*
  term: factor (('*'|'@'|'/'|'%'|'//') factor)*
  factor: ('+'|'-'|'~') factor | power
  power: atom_expr ['**' factor]
  atom_expr: ['await'] atom trailer*
  atom: ('(' [yield_expr|testlist_comp] ')' |
         '[' [testlist_comp] ']' |
         '{' [dictorsetmaker] '}' |
         NAME | NUMBER | STRING+ | '...' | 'None' | 'True' | 'False')
  testlist_comp: (test|star_expr) ( comp_for | (',' (test|star_expr))* [','] )
  trailer: '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME
  subscriptlist: subscript (',' subscript)* [',']
  subscript: test | [test] ':' [test] [sliceop]
  sliceop: ':' [test]
  exprlist: (expr|star_expr) (',' (expr|star_expr))* [',']
  testlist: test (',' test)* [',']
  dictorsetmaker: ( ((test ':' test | '**' expr)
                     (comp_for | (',' (test ':' test | '**' expr))* [','])) |
                    ((test | star_expr)
                     (comp_for | (',' (test | star_expr))* [','])) )

  classdef: 'class' NAME ['(' [arglist] ')'] ':' suite

  arglist: argument (',' argument)*  [',']

  # The reason that keywords are test nodes instead of NAME is that using NAME
  # results in an ambiguity. ast.c makes sure it's a NAME.
  # "test '=' test" is really "keyword '=' test", but we have no such token.
  # These need to be in a single rule to avoid grammar that is ambiguous
  # to our LL(1) parser. Even though 'test' includes '*expr' in star_expr,
  # we explicitly match '*' here, too, to give it proper precedence.
  # Illegal combinations and orderings are blocked in ast.c:
  # multiple (test comp_for) arguments are blocked; keyword unpackings
  # that precede iterable unpackings are blocked; etc.
  argument: ( test [comp_for] |
              test '=' test |
              '**' test |
              '*' test )

  comp_iter: comp_for | comp_if
  sync_comp_for: 'for' exprlist 'in' or_test [comp_iter]
  comp_for: ['async'] sync_comp_for
  comp_if: 'if' test_nocond [comp_iter]

  # not used in grammar, but may appear in "node" passed from Parser to Compiler

  yield_expr: 'yield' [yield_arg]
  yield_arg: 'from' test | testlist
  *)
  let parse =
    let open Python_tokens_base in
    assert false

  let parser : P2.token Stream.t -> int =
    P2.parse (P2.type_check parse)

  let unstaged_complete : string -> int =
    fun s -> assert false
end
