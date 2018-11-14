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
  let (!~) = tok

  let break_stmt = !~KW_break
  let continue_stmt = !~KW_continue
  let vfpdef = !~NAME
  let pass_stmt = !~KW_pass
  let dotted_name = !~NAME >>> star (!~DOT >>> !~NAME)
  let global_stmt = !~KW_global >>> !~NAME >>>
                      star (!~COMMA >>> !~NAME)
  let nonlocal_stmt = !~KW_nonlocal >>> !~NAME >>>
                      star (!~COMMA >>> !~NAME)
  let encoding_decl = !~NAME

  let dotted_as_name = dotted_name >>> maybe (!~KW_as >>> !~NAME)
  let dotted_as_names =  dotted_as_name >>> star (!~COMMA >>> dotted_as_name)
  let import_name = !~KW_import >>> dotted_as_names

  let import_as_name = !~NAME >>> maybe (!~KW_as >>> !~NAME)

  let import_as_names = import_as_name >>> star (!~COMMA >>> import_as_name) >>>
                          maybe (!~COMMA)

  let import_from = !~KW_from >>>
                    ((star (!~DOT <+> !~ELLIPSIS) >>> dotted_name)
                   <+> plus (!~DOT <+> !~ELLIPSIS)) >>>
                    !~KW_import >>>
                    (!~STAR
                  <+> (!~LPAR >>> import_as_names >>> !~RPAR)
                  <+> import_as_names)

  let import_stmt = import_name <+> import_from

  let augassign =
    any (List.map (!~) [
             PLUSEQUAL; MINEQUAL; STAREQUAL; ATEQUAL; SLASHEQUAL; PERCENTEQUAL;
             AMPEREQUAL; VBAREQUAL; CIRCUMFLEXEQUAL; LEFTSHIFTEQUAL;
             RIGHTSHIFTEQUAL; DOUBLESTAREQUAL; DOUBLESTAREQUAL
      ])

  let comp_op =
    any (List.map (!~) [
          LESS; GREATER; EQEQUAL; GREATEREQUAL; LESSEQUAL; NOTEQUAL;
          KW_in;
         ])
      <+> (!~KW_not >>> !~KW_in)
      <+> !~KW_is
      <+> (!~KW_is >>> !~KW_not)


  module Python_index =
  struct
    open Fixes

    type _ t =
      | Single_input       : unit t
      | File_input         : unit t
      | Eval_input         : unit t
      | Decorator          : unit t
      | Decorators         : unit t
      | Decorated          : unit t
      | Async_funcdef      : unit t
      | Funcdef            : unit t
      | Parameters         : unit t
      | Typedargslist      : unit t
      | Tfpdef             : unit t
      | Varargslist        : unit t
      | Stmt               : unit t
      | Simple_stmt        : unit t
      | Small_stmt         : unit t
      | Expr_stmt          : unit t
      | Annassign          : unit t
      | Testlist_star_expr : unit t
      | Del_stmt           : unit t
      | Flow_stmt          : unit t
      | Return_stmt        : unit t
      | Yield_stmt         : unit t
      | Raise_stmt         : unit t
      | Assert_stmt        : unit t
      | Compound_stmt      : unit t
      | Async_stmt         : unit t
      | If_stmt            : unit t
      | While_stmt         : unit t
      | For_stmt           : unit t
      | Try_stmt           : unit t
      | With_stmt          : unit t
      | With_item          : unit t
      | Except_clause      : unit t
      | Suite              : unit t
      | Test               : unit t
      | Test_nocond        : unit t
      | Lambdef            : unit t
      | Lambdef_nocond     : unit t
      | Or_test            : unit t
      | And_test           : unit t
      | Not_test           : unit t
      | Comparison         : unit t
      | Star_expr          : unit t
      | Expr               : unit t
      | Xor_expr           : unit t
      | And_expr           : unit t
      | Shift_expr         : unit t
      | Arith_expr         : unit t
      | Term               : unit t
      | Factor             : unit t
      | Power              : unit t
      | Atom_expr          : unit t
      | Atom               : unit t
      | Testlist_comp      : unit t
      | Trailer            : unit t
      | Subscriptlist      : unit t
      | Subscript          : unit t
      | Sliceop            : unit t
      | Exprlist           : unit t
      | Testlist           : unit t
      | Dictorsetmaker     : unit t
      | Classdef           : unit t
      | Arglist            : unit t
      | Argument           : unit t
      | Comp_iter          : unit t
      | Sync_comp_for      : unit t
      | Comp_for           : unit t
      | Comp_if            : unit t
      | Yield_expr         : unit t
      | Yield_arg          : unit t

     let equalp : type a b. a t -> b t -> (a, b) eql option =
      fun l r -> match l, r with
      | Single_input       , Single_input       -> Some Refl
      | File_input         , File_input         -> Some Refl
      | Eval_input         , Eval_input         -> Some Refl
      | Decorator          , Decorator          -> Some Refl
      | Decorators         , Decorators         -> Some Refl
      | Decorated          , Decorated          -> Some Refl
      | Async_funcdef      , Async_funcdef      -> Some Refl
      | Funcdef            , Funcdef            -> Some Refl
      | Parameters         , Parameters         -> Some Refl
      | Typedargslist      , Typedargslist      -> Some Refl
      | Tfpdef             , Tfpdef             -> Some Refl
      | Varargslist        , Varargslist        -> Some Refl
      | Stmt               , Stmt               -> Some Refl
      | Simple_stmt        , Simple_stmt        -> Some Refl
      | Small_stmt         , Small_stmt         -> Some Refl
      | Expr_stmt          , Expr_stmt          -> Some Refl
      | Annassign          , Annassign          -> Some Refl
      | Testlist_star_expr , Testlist_star_expr -> Some Refl
      | Del_stmt           , Del_stmt           -> Some Refl
      | Flow_stmt          , Flow_stmt          -> Some Refl
      | Return_stmt        , Return_stmt        -> Some Refl
      | Yield_stmt         , Yield_stmt         -> Some Refl
      | Raise_stmt         , Raise_stmt         -> Some Refl
      | Assert_stmt        , Assert_stmt        -> Some Refl
      | Compound_stmt      , Compound_stmt      -> Some Refl
      | Async_stmt         , Async_stmt         -> Some Refl
      | If_stmt            , If_stmt            -> Some Refl
      | While_stmt         , While_stmt         -> Some Refl
      | For_stmt           , For_stmt           -> Some Refl
      | Try_stmt           , Try_stmt           -> Some Refl
      | With_stmt          , With_stmt          -> Some Refl
      | With_item          , With_item          -> Some Refl
      | Except_clause      , Except_clause      -> Some Refl
      | Suite              , Suite              -> Some Refl
      | Test               , Test               -> Some Refl
      | Test_nocond        , Test_nocond        -> Some Refl
      | Lambdef            , Lambdef            -> Some Refl
      | Lambdef_nocond     , Lambdef_nocond     -> Some Refl
      | Or_test            , Or_test            -> Some Refl
      | And_test           , And_test           -> Some Refl
      | Not_test           , Not_test           -> Some Refl
      | Comparison         , Comparison         -> Some Refl
      | Star_expr          , Star_expr          -> Some Refl
      | Expr               , Expr               -> Some Refl
      | Xor_expr           , Xor_expr           -> Some Refl
      | And_expr           , And_expr           -> Some Refl
      | Shift_expr         , Shift_expr         -> Some Refl
      | Arith_expr         , Arith_expr         -> Some Refl
      | Term               , Term               -> Some Refl
      | Factor             , Factor             -> Some Refl
      | Power              , Power              -> Some Refl
      | Atom_expr          , Atom_expr          -> Some Refl
      | Atom               , Atom               -> Some Refl
      | Testlist_comp      , Testlist_comp      -> Some Refl
      | Trailer            , Trailer            -> Some Refl
      | Subscriptlist      , Subscriptlist      -> Some Refl
      | Subscript          , Subscript          -> Some Refl
      | Sliceop            , Sliceop            -> Some Refl
      | Exprlist           , Exprlist           -> Some Refl
      | Testlist           , Testlist           -> Some Refl
      | Dictorsetmaker     , Dictorsetmaker    -> Some Refl
      | Classdef           , Classdef           -> Some Refl
      | Arglist            , Arglist            -> Some Refl
      | Argument           , Argument           -> Some Refl
      | Comp_iter          , Comp_iter          -> Some Refl
      | Sync_comp_for      , Sync_comp_for      -> Some Refl
      | Comp_for           , Comp_for           -> Some Refl
      | Comp_if            , Comp_if            -> Some Refl
      | Yield_expr         , Yield_expr         -> Some Refl
      | Yield_arg          , Yield_arg          -> Some Refl
      | _                                       -> None
  end

  module Python_fix = Fixes.Fix(Python_index)

  let parse =
    let open Python_index in
    let open Python_fix in
    let r : type a. resolve -> a t -> a P2.t =
      fun { resolve=(!); _ } -> function
      (*
        # Start symbols for the grammar:
        #  single_input is a single interactive statement;
        #  file_input is a module or sequence of commands read from an input file;
        #  eval_input is the input for the eval() functions.
        # NB: compound_stmt in single_input is followed by extra NEWLINE!
       *)
       | Single_input -> (!~NEWLINE $ ignore)
                     <|> (!Simple_stmt $ ignore)
                     <|> (!Compound_stmt >>> !~NEWLINE $ ignore)
       | File_input -> (star (!~NEWLINE <|> !Stmt) >>> !~ENDMARKER)
                     $ ignore
       | Eval_input -> (!Testlist >>> star (!~NEWLINE) >>> !~ENDMARKER)
                     $ ignore
       | Decorator -> !~AT >>>
                      dotted_name >>>
                      maybe (!~LPAR >>>
                           maybe !Arglist >>>
                           !~RPAR) >>>
                      !~NEWLINE
                    $ ignore
       | Decorators -> star !Decorator $ ignore
       | Decorated -> !Decorators >>>
                       (!Classdef
                    <|> !Funcdef
                    <|> !Async_funcdef) $ ignore
       | Async_funcdef -> (!~KW_async >>> !Funcdef) $ ignore
       | Funcdef -> !~KW_def >>>
                    !~NAME >>>
                    !Parameters >>>
                    maybe (!~RARROW >>> !Test) >>> 
                    !~COLON >>>
                    !Suite
                  $ ignore
       | Parameters -> !~LPAR >>> maybe !Typedargslist >>> !~RPAR
                     $ ignore
       | Typedargslist ->
          let tfpdefeqtest = !Tfpdef >>> maybe (!~EQUAL >>> !Test) $ ignore in
          let doublestarbit = !~DOUBLESTAR >>> !Tfpdef >>> maybe !~COMMA $ ignore in
          let starbit = !~STAR >>> maybe !Tfpdef >>> star (!~COMMA >>> tfpdefeqtest) >>>
                          maybe (!~COMMA >>> maybe doublestarbit) $ ignore
          in
          (tfpdefeqtest >>> star (!~COMMA >>> tfpdefeqtest) >>>
             maybe (!~COMMA >>> maybe (starbit <|> doublestarbit))) $ ignore
          <|> starbit
          <|> doublestarbit
          $ ignore
       | Varargslist ->
          (* NB: the structure is identical to Typedargslist. We could abstract *)
          let tfpdefeqtest = !Tfpdef >>> maybe (!~EQUAL >>> !Test) $ ignore in
          let doublestarbit = !~DOUBLESTAR >>> !Tfpdef >>> maybe !~COMMA $ ignore in
          let starbit = !~STAR >>> maybe !Tfpdef >>> star (!~COMMA >>> tfpdefeqtest) >>>
                          maybe (!~COMMA >>> maybe doublestarbit) $ ignore
          in
          (tfpdefeqtest >>> star (!~COMMA >>> tfpdefeqtest) >>>
             maybe (!~COMMA >>> maybe (starbit <|> doublestarbit))) $ ignore
          <|> starbit
          <|> doublestarbit
          $ ignore
       | Stmt -> !Simple_stmt <|> !Compound_stmt $ ignore
       | Simple_stmt -> !Small_stmt >>> star (!~SEMI >>> !Small_stmt) >>> maybe !~SEMI >>> !~NEWLINE
                      $ ignore
       | Small_stmt -> !Expr_stmt <|> !Del_stmt <|> (pass_stmt $ ignore) <|> !Flow_stmt
                   <|> (import_stmt $ ignore) <|> (global_stmt $ ignore) <|> (nonlocal_stmt $ ignore) <|> !Assert_stmt
                     $ ignore
       | Expr_stmt -> !Testlist_star_expr >>>
                        star (!Annassign
                         <|> (augassign >>> (!Yield_expr <|> !Testlist) $ ignore)
                         <|> (!Yield_expr <|> !Testlist_star_expr))
                    $ ignore
       | Annassign -> !~COLON >>> !Test >>> maybe (!~EQUAL >>> !Test)
                    $ ignore
       | Testlist_star_expr -> (!Test <|> !Star_expr) >>>
                               star (!~COMMA >>> (!Test <|> !Star_expr)) >>>
                               maybe !~COMMA
                             $ ignore
       | Del_stmt -> !~KW_del >>> !Exprlist $ ignore
       | Flow_stmt -> break_stmt <|> continue_stmt <|> !Return_stmt <|> !Raise_stmt <|> !Yield_stmt $ ignore
       | Return_stmt -> !~KW_return >>> maybe !Testlist $ ignore
       | Yield_stmt -> !Yield_expr
       | Raise_stmt -> !~KW_raise >>> maybe (!Test >>> maybe (!~KW_from >>> !Test)) $ ignore
       | Assert_stmt -> !~KW_assert >>> !Test >>> maybe (!~COMMA >>> !Test) $ ignore
       | Compound_stmt -> !If_stmt <|> !While_stmt <|> !For_stmt <|> !Try_stmt
                      <|> !With_stmt <|> !Funcdef <|> !Classdef <|> !Decorated
                      <|> !Async_stmt $ ignore
       | Async_stmt -> !~KW_async >>> (!Funcdef <|> !With_stmt <|> !For_stmt) $ ignore
       | If_stmt -> !~KW_if >>> !Test >>> !~COLON >>> !Suite >>>
                      star (!~KW_elif >>> !Test >>> !~COLON >>> !Suite) >>>
                      maybe (!~KW_else >>> !~COLON >>> !Suite)
                  $ ignore
       | While_stmt -> !~KW_while >>> !Test >>> !~COLON >>> !Suite >>>
                         maybe (!~KW_else >>> !~COLON >>> !Suite)
                     $ ignore
       | For_stmt -> !~KW_for >>> !Exprlist >>> !~KW_in >>> !Testlist >>> !~COLON >>>
                       !Suite >>> maybe (!~KW_else >>> !~COLON >>> !Suite)
                     $ ignore
       | Try_stmt -> !~KW_try >>> !~COLON >>> !Suite >>>
                       ((plus (!Except_clause >>> !~COLON >>> !Suite) >>>
                        maybe (!~KW_else >>> !~COLON >>> !Suite) >>>
                           maybe (!~KW_finally >>> !~COLON >>> !Suite) $ ignore)
                         <|> (!~KW_finally >>> !~COLON >>> !Suite $ ignore))
                     $ ignore
       | With_stmt -> !~KW_with >>> !With_item >>> star (!~COMMA >>> !With_item) >>> !~COLON >>> !Suite
                    $ ignore
       | With_item -> !Test >>> maybe (!~KW_as >>> !Expr) $ ignore
       | Except_clause -> !~KW_except >>> (maybe !Test >>> (maybe (!~KW_as >>> !~NAME))) $ ignore
       | Suite -> !Simple_stmt <|> !~NEWLINE >>> !~INDENT >>> plus !Stmt >>> !~DEDENT $ ignore
       | Test -> (!Or_test >>> maybe (!~KW_if >>> !Or_test >>> !~KW_else >>> !Test) $ ignore)
                 <+> !Lambdef $ ignore
       | Test_nocond -> !Or_test <|> !Lambdef_nocond $ ignore
       | Lambdef -> !~KW_lambda >>> maybe !Varargslist >>> !~COLON >>> !Test $ ignore
       | Lambdef_nocond -> !~KW_lambda >>> maybe !Varargslist >>> !~COLON >>> !Test_nocond $ ignore
       | Or_test -> !And_test >>> star (!~KW_or >>> !And_test) $ ignore
       | And_test -> !Not_test >>> star (!~KW_and >>> !Not_test) $ ignore
       | Not_test -> (!~KW_not >>> !Not_test) <+> !Comparison $ ignore
       | Comparison -> !Expr >>> star (comp_op >>> !Expr) $ ignore
       | Star_expr -> !~STAR >>> !Expr $ ignore
       | Expr -> !Xor_expr >>> star (!~VBAR >>> !Xor_expr) $ ignore
       | Xor_expr -> !And_expr >>> star (!~CIRCUMFLEX >>> !And_expr) $ ignore
       | And_expr -> !Shift_expr >>> star (!~AMPER >>> !Shift_expr) $ ignore
       | Shift_expr -> !Arith_expr >>> star ((!~LEFTSHIFT <|> !~RIGHTSHIFT) >>> !Arith_expr) $ ignore
       | Arith_expr -> !Term >>> star ((!~PLUS <|> !~MINUS) >>> !Term) $ ignore
       | Term -> !Factor >>>
                   star ((!~STAR <|> !~AT <|> !~SLASH <|> !~PERCENT <|> !~DOUBLESLASH) >>> !Factor)
                 $ ignore
       | Factor -> ((!~PLUS <|> !~MINUS <|> !~TILDE) >>> !Factor $ ignore) <|> !Power $ ignore
       | Power -> !Atom_expr >>> maybe (!~DOUBLESTAR >>> !Factor) $ ignore
       | Atom_expr -> !~KW_await >>> !Atom >>> star !Trailer $ ignore
       | Atom -> ((!~LPAR >>> maybe (!Yield_expr <|> !Testlist_comp) >>> !~RPAR $ ignore) <|>
           (!~LSQB >>> maybe !Testlist_comp >>> !~RSQB $ ignore) <|>
           (!~LBRACE >>> maybe !Dictorsetmaker >>> !~RBRACE $ ignore) <|>
           (!~NAME $ ignore) <|> (!~NUMBER $ ignore)
        <|> ((plus !~STRING) $ ignore) <|> !~ELLIPSIS <|> !~KW_None <|> !~KW_True <|> !~KW_False) 
               $ ignore
       | Testlist_comp -> (!Test <|> !Star_expr $ ignore) >>>
                          (!Comp_for <|> (star (!~COMMA >>> (!Test <|> !Star_expr) >>> maybe !~COMMA) $ ignore))
                         $ ignore
       | Trailer -> (!~LPAR >>> maybe !Arglist >>> !~RPAR $ ignore)
                <|> (!~LSQB >>> maybe !Subscriptlist >>> !~RSQB $ ignore)
                <|> (!~DOT >>> !~NAME $ ignore)
       | Subscriptlist -> !Subscript >>> star (!~COMMA >>> !Subscript) >>> maybe !~COMMA $ ignore
       | Subscript -> !Test
                  <|> (maybe !Test >>> !~COLON >>> maybe !Test >>> maybe !Sliceop $ ignore)
       | Sliceop -> !~COLON >>> maybe !Test $ ignore
       | Exprlist -> (!Expr <|> !Star_expr) >>> star (!~COMMA >>> (!Expr <|> !Star_expr)) >>> maybe !~COMMA
                   $ ignore
       | Testlist -> !Test >>> star (!~COMMA >>> !Test) >>> maybe !~COMMA
                     $ ignore
       | Dictorsetmaker -> (((!Test >>> !~COLON >>> !Test $ ignore) <|> (!~DOUBLESTAR >>> !Expr $ ignore)) >>>
                             (!Comp_for <|> (star (!~COMMA >>> (!Test >>> !~COLON >>> !Test) $ ignore <|> (!~DOUBLESTAR >>> !Expr $ ignore)) >>> maybe !~COMMA $ ignore)))
                           $ ignore
                           <|>
                             ((!Test <|> !Star_expr) >>>
                             (!Comp_for <|> (star (!~COMMA >>> (!Test <|> !Star_expr)) >>> maybe !~COMMA $ ignore)) $ ignore)
                         $ ignore
       | Classdef -> !~KW_class >>> !~NAME >>>
                       maybe (!~LPAR >>> maybe !Arglist >>> !~RPAR) >>>
                       !~COLON >>> !Suite
                   $ ignore
       | Arglist -> !Argument >>> star (!~COMMA >>> !Argument) >>> maybe !~COMMA
                    $ ignore
       | Argument -> (!Test >>> maybe !Comp_for)
                 <+> (!Test >>> !~EQUAL >>> !Test)
                 <+> (!~DOUBLESTAR >>> !Test)
                 <+> (!~STAR >>> !Test)
                   $ ignore
       | Comp_iter -> !Comp_for <|> !Comp_if
       | Sync_comp_for -> !~KW_for >>> !Exprlist >>> !~KW_in >>> !Or_test >>> maybe !Comp_iter $ ignore
       | Comp_for -> maybe !~KW_async >>> !Sync_comp_for $ ignore
       | Comp_if -> !~KW_if >>> !Test_nocond >>> maybe !Comp_iter $ ignore
       | Yield_expr -> !~KW_yield >>> maybe !Yield_arg $ ignore
       | Yield_arg -> (!~KW_from >>> !Test $ ignore) <|> !Testlist
    in fixn { r } Stmt

  let parser : P2.token Stream.t -> unit =
    P2.parse (P2.type_check parse)

  let unstaged_complete : string -> int =
    fun s -> assert false
end
