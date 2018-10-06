(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

include Python_tokens_base

let within_ : utag -> utag -> t code -> bool code =
  fun l h x ->
  let l = Python_tokens_base.to_int l and h = Python_tokens_base.to_int h in
  if l = h then .< Python_tokens_base.to_int (Python_tokens_base.tag .~x) = l >.
  else  .< l <= Python_tokens_base.to_int (Python_tokens_base.tag .~x) && Python_tokens_base.to_int (Python_tokens_base.tag .~x) <= h >.

let pp_utag fmt (U x) = Tag.print fmt x

let lift_tag : type a. a tag -> a tag code = function
  | ENDMARKER        -> .< Python_tokens_base.ENDMARKER >.
  | NAME             -> .< Python_tokens_base.NAME >.
  | NUMBER           -> .< Python_tokens_base.NUMBER >.
  | STRING           -> .< Python_tokens_base.STRING >.
  | NEWLINE          -> .< Python_tokens_base.NEWLINE >.
  | INDENT           -> .< Python_tokens_base.INDENT >.
  | DEDENT           -> .< Python_tokens_base.DEDENT >.
  | LPAR             -> .< Python_tokens_base.LPAR >.
  | RPAR             -> .< Python_tokens_base.RPAR >.
  | LSQB             -> .< Python_tokens_base.LSQB >.
  | RSQB             -> .< Python_tokens_base.RSQB >.
  | COLON            -> .< Python_tokens_base.COLON >.
  | COMMA            -> .< Python_tokens_base.COMMA >.
  | SEMI             -> .< Python_tokens_base.SEMI >.
  | PLUS             -> .< Python_tokens_base.PLUS >.
  | MINUS            -> .< Python_tokens_base.MINUS >.
  | STAR             -> .< Python_tokens_base.STAR >.
  | SLASH            -> .< Python_tokens_base.SLASH >.
  | VBAR             -> .< Python_tokens_base.VBAR >.
  | AMPER            -> .< Python_tokens_base.AMPER >.
  | LESS             -> .< Python_tokens_base.LESS >.
  | GREATER          -> .< Python_tokens_base.GREATER >.
  | EQUAL            -> .< Python_tokens_base.EQUAL >.
  | DOT              -> .< Python_tokens_base.DOT >.
  | PERCENT          -> .< Python_tokens_base.PERCENT >.
  | LBRACE           -> .< Python_tokens_base.LBRACE >.
  | RBRACE           -> .< Python_tokens_base.RBRACE >.
  | EQEQUAL          -> .< Python_tokens_base.EQEQUAL >.
  | NOTEQUAL         -> .< Python_tokens_base.NOTEQUAL >.
  | LESSEQUAL        -> .< Python_tokens_base.LESSEQUAL >.
  | GREATEREQUAL     -> .< Python_tokens_base.GREATEREQUAL >.
  | TILDE            -> .< Python_tokens_base.TILDE >.
  | CIRCUMFLEX       -> .< Python_tokens_base.CIRCUMFLEX >.
  | LEFTSHIFT        -> .< Python_tokens_base.LEFTSHIFT >.
  | RIGHTSHIFT       -> .< Python_tokens_base.RIGHTSHIFT >.
  | DOUBLESTAR       -> .< Python_tokens_base.DOUBLESTAR >.
  | PLUSEQUAL        -> .< Python_tokens_base.PLUSEQUAL >.
  | MINEQUAL         -> .< Python_tokens_base.MINEQUAL >.
  | STAREQUAL        -> .< Python_tokens_base.STAREQUAL >.
  | SLASHEQUAL       -> .< Python_tokens_base.SLASHEQUAL >.
  | PERCENTEQUAL     -> .< Python_tokens_base.PERCENTEQUAL >.
  | AMPEREQUAL       -> .< Python_tokens_base.AMPEREQUAL >.
  | VBAREQUAL        -> .< Python_tokens_base.VBAREQUAL >.
  | CIRCUMFLEXEQUAL  -> .< Python_tokens_base.CIRCUMFLEXEQUAL >.
  | LEFTSHIFTEQUAL   -> .< Python_tokens_base.LEFTSHIFTEQUAL >.
  | RIGHTSHIFTEQUAL  -> .< Python_tokens_base.RIGHTSHIFTEQUAL >.
  | DOUBLESTAREQUAL  -> .< Python_tokens_base.DOUBLESTAREQUAL >.
  | DOUBLESLASH      -> .< Python_tokens_base.DOUBLESLASH >.
  | DOUBLESLASHEQUAL -> .< Python_tokens_base.DOUBLESLASHEQUAL >.
  | AT               -> .< Python_tokens_base.AT >.
  | ATEQUAL          -> .< Python_tokens_base.ATEQUAL >.
  | RARROW           -> .< Python_tokens_base.RARROW >.
  | ELLIPSIS         -> .< Python_tokens_base.ELLIPSIS >.
  | OP               -> .< Python_tokens_base.OP >.
  | ERRORTOKEN       -> .< Python_tokens_base.ERRORTOKEN >.
  | COMMENT          -> .< Python_tokens_base.COMMENT >.
  | NL               -> .< Python_tokens_base.NL >.
  | ENCODING         -> .< Python_tokens_base.ENCODING >.
  | N_TOKENS         -> .< Python_tokens_base.N_TOKENS >.

  | KW_and           -> .< Python_tokens_base.KW_and >.
  | KW_as            -> .< Python_tokens_base.KW_as >.
  | KW_assert        -> .< Python_tokens_base.KW_assert >.
  | KW_async         -> .< Python_tokens_base.KW_async >.
  | KW_await         -> .< Python_tokens_base.KW_await >.
  | KW_break         -> .< Python_tokens_base.KW_break >.
  | KW_class         -> .< Python_tokens_base.KW_class >.
  | KW_continue      -> .< Python_tokens_base.KW_continue >.
  | KW_def           -> .< Python_tokens_base.KW_def >.
  | KW_del           -> .< Python_tokens_base.KW_del >.
  | KW_elif          -> .< Python_tokens_base.KW_elif >.
  | KW_else          -> .< Python_tokens_base.KW_else >.
  | KW_except        -> .< Python_tokens_base.KW_except >.
  | KW_False         -> .< Python_tokens_base.KW_False >.
  | KW_finally       -> .< Python_tokens_base.KW_finally >.
  | KW_for           -> .< Python_tokens_base.KW_for >.
  | KW_from          -> .< Python_tokens_base.KW_from >.
  | KW_global        -> .< Python_tokens_base.KW_global >.
  | KW_if            -> .< Python_tokens_base.KW_if >.
  | KW_import        -> .< Python_tokens_base.KW_import >.
  | KW_in            -> .< Python_tokens_base.KW_in >.
  | KW_is            -> .< Python_tokens_base.KW_is >.
  | KW_lambda        -> .< Python_tokens_base.KW_lambda >.
  | KW_None          -> .< Python_tokens_base.KW_None >.
  | KW_nonlocal      -> .< Python_tokens_base.KW_nonlocal >.
  | KW_not           -> .< Python_tokens_base.KW_not >.
  | KW_or            -> .< Python_tokens_base.KW_or >.
  | KW_pass          -> .< Python_tokens_base.KW_pass >.
  | KW_raise         -> .< Python_tokens_base.KW_raise >.
  | KW_return        -> .< Python_tokens_base.KW_return >.
  | KW_True          -> .< Python_tokens_base.KW_True >.
  | KW_try           -> .< Python_tokens_base.KW_try >.
  | KW_while         -> .< Python_tokens_base.KW_while >.
  | KW_with          -> .< Python_tokens_base.KW_with >.
  | KW_yield         -> .< Python_tokens_base.KW_yield >.


let rec lift_list f = function
  | [] -> .< [] >.
  | x :: xs -> .< .~(f x) :: .~(lift_list f xs) >.

let test_tag : type a b. complete:bool -> a tag list -> t code ->
                    (a code option -> b code) ->
                    b code =
  fun ~complete tags x k ->
  if complete then k (Some .< snd (Obj.magic .~x) >.) else
  match tags with
    [tag] ->
 .< Python_tokens_base.match_ .~x .~(lift_tag tag)
                    (fun x -> .~(k (Some .<x>.)))
                    (fun () -> .~(k None)) >.
  | tags ->
 .< Python_tokens_base.matchlist_ .~x .~(lift_list lift_tag tags)
                    (fun x -> .~(k (Some .<x>.)))
                    (fun () -> .~(k None)) >.
