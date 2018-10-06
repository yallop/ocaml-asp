
(** These must match the definitions in token.h *)
type _ tag =
  | ENDMARKER        : unit tag   (* 0  *)
  | NAME             : string tag (* 1  *)
  | NUMBER           : string tag (* 2  *)
  | STRING           : string tag (* 3  *)
  | NEWLINE          : unit tag   (* 4  *)
  | INDENT           : unit tag   (* 5  *)
  | DEDENT           : unit tag   (* 6  *)
  | LPAR             : unit tag   (* 7  *)
  | RPAR             : unit tag   (* 8  *)
  | LSQB             : unit tag   (* 9  *)
  | RSQB             : unit tag   (* 10 *)
  | COLON            : unit tag   (* 11 *)
  | COMMA            : unit tag   (* 12 *)
  | SEMI             : unit tag   (* 13 *)
  | PLUS             : unit tag   (* 14 *)
  | MINUS            : unit tag   (* 15 *)
  | STAR             : unit tag   (* 16 *)
  | SLASH            : unit tag   (* 17 *)
  | VBAR             : unit tag   (* 18 *)
  | AMPER            : unit tag   (* 19 *)
  | LESS             : unit tag   (* 20 *)
  | GREATER          : unit tag   (* 21 *)
  | EQUAL            : unit tag   (* 22 *)
  | DOT              : unit tag   (* 23 *)
  | PERCENT          : unit tag   (* 24 *)
  | LBRACE           : unit tag   (* 25 *)
  | RBRACE           : unit tag   (* 26 *)
  | EQEQUAL          : unit tag   (* 27 *)
  | NOTEQUAL         : unit tag   (* 28 *)
  | LESSEQUAL        : unit tag   (* 29 *)
  | GREATEREQUAL     : unit tag   (* 30 *)
  | TILDE            : unit tag   (* 31 *)
  | CIRCUMFLEX       : unit tag   (* 32 *)
  | LEFTSHIFT        : unit tag   (* 33 *)
  | RIGHTSHIFT       : unit tag   (* 34 *)
  | DOUBLESTAR       : unit tag   (* 35 *)
  | PLUSEQUAL        : unit tag   (* 36 *)
  | MINEQUAL         : unit tag   (* 37 *)
  | STAREQUAL        : unit tag   (* 38 *)
  | SLASHEQUAL       : unit tag   (* 39 *)
  | PERCENTEQUAL     : unit tag   (* 40 *)
  | AMPEREQUAL       : unit tag   (* 41 *)
  | VBAREQUAL        : unit tag   (* 42 *)
  | CIRCUMFLEXEQUAL  : unit tag   (* 43 *)
  | LEFTSHIFTEQUAL   : unit tag   (* 44 *)
  | RIGHTSHIFTEQUAL  : unit tag   (* 45 *)
  | DOUBLESTAREQUAL  : unit tag   (* 46 *)
  | DOUBLESLASH      : unit tag   (* 47 *)
  | DOUBLESLASHEQUAL : unit tag   (* 48 *)
  | AT               : unit tag   (* 49 *)
  | ATEQUAL          : unit tag   (* 50 *)
  | RARROW           : unit tag   (* 51 *)
  | ELLIPSIS         : unit tag   (* 52 *)
  | OP               : string tag (* 53 *)
  | ERRORTOKEN       : unit tag   (* 54 *)
  | COMMENT          : unit tag   (* 55 *)
  | NL               : unit tag   (* 56 *)
  | ENCODING         : unit tag   (* 57 *)
  | N_TOKENS         : unit tag   (* 58 *)
  (* pseudo-keywords: treated as "names" by the tokenizer, but
   a s individual non-terminals by the parser. *)
  | KW_and           : unit tag
  | KW_as            : unit tag
  | KW_assert        : unit tag
  | KW_async         : unit tag
  | KW_await         : unit tag
  | KW_break         : unit tag
  | KW_class         : unit tag
  | KW_continue      : unit tag
  | KW_def           : unit tag
  | KW_del           : unit tag
  | KW_elif          : unit tag
  | KW_else          : unit tag
  | KW_except        : unit tag
  | KW_False         : unit tag
  | KW_finally       : unit tag
  | KW_for           : unit tag
  | KW_from          : unit tag
  | KW_global        : unit tag
  | KW_if            : unit tag
  | KW_import        : unit tag
  | KW_in            : unit tag
  | KW_is            : unit tag
  | KW_lambda        : unit tag
  | KW_None          : unit tag
  | KW_nonlocal      : unit tag
  | KW_not           : unit tag
  | KW_or            : unit tag
  | KW_pass          : unit tag
  | KW_raise         : unit tag
  | KW_return        : unit tag
  | KW_True          : unit tag
  | KW_try           : unit tag
  | KW_while         : unit tag
  | KW_with          : unit tag
  | KW_yield         : unit tag


module Tag =
struct
  type 'a t = 'a tag

  let print : type a. Format.formatter -> a t -> unit =
    fun fmt -> function
     | ENDMARKER        -> Format.fprintf fmt "ENDMARKER"
     | NAME             -> Format.fprintf fmt "NAME"
     | NUMBER           -> Format.fprintf fmt "NUMBER"
     | STRING           -> Format.fprintf fmt "STRING"
     | NEWLINE          -> Format.fprintf fmt "NEWLINE"
     | INDENT           -> Format.fprintf fmt "INDENT"
     | DEDENT           -> Format.fprintf fmt "DEDENT"
     | LPAR             -> Format.fprintf fmt "LPAR"
     | RPAR             -> Format.fprintf fmt "RPAR"
     | LSQB             -> Format.fprintf fmt "LSQB"
     | RSQB             -> Format.fprintf fmt "RSQB"
     | COLON            -> Format.fprintf fmt "COLON"
     | COMMA            -> Format.fprintf fmt "COMMA"
     | SEMI             -> Format.fprintf fmt "SEMI"
     | PLUS             -> Format.fprintf fmt "PLUS"
     | MINUS            -> Format.fprintf fmt "MINUS"
     | STAR             -> Format.fprintf fmt "STAR"
     | SLASH            -> Format.fprintf fmt "SLASH"
     | VBAR             -> Format.fprintf fmt "VBAR"
     | AMPER            -> Format.fprintf fmt "AMPER"
     | LESS             -> Format.fprintf fmt "LESS"
     | GREATER          -> Format.fprintf fmt "GREATER"
     | EQUAL            -> Format.fprintf fmt "EQUAL"
     | DOT              -> Format.fprintf fmt "DOT"
     | PERCENT          -> Format.fprintf fmt "PERCENT"
     | LBRACE           -> Format.fprintf fmt "LBRACE"
     | RBRACE           -> Format.fprintf fmt "RBRACE"
     | EQEQUAL          -> Format.fprintf fmt "EQEQUAL"
     | NOTEQUAL         -> Format.fprintf fmt "NOTEQUAL"
     | LESSEQUAL        -> Format.fprintf fmt "LESSEQUAL"
     | GREATEREQUAL     -> Format.fprintf fmt "GREATEREQUAL"
     | TILDE            -> Format.fprintf fmt "TILDE"
     | CIRCUMFLEX       -> Format.fprintf fmt "CIRCUMFLEX"
     | LEFTSHIFT        -> Format.fprintf fmt "LEFTSHIFT"
     | RIGHTSHIFT       -> Format.fprintf fmt "RIGHTSHIFT"
     | DOUBLESTAR       -> Format.fprintf fmt "DOUBLESTAR"
     | PLUSEQUAL        -> Format.fprintf fmt "PLUSEQUAL"
     | MINEQUAL         -> Format.fprintf fmt "MINEQUAL"
     | STAREQUAL        -> Format.fprintf fmt "STAREQUAL"
     | SLASHEQUAL       -> Format.fprintf fmt "SLASHEQUAL"
     | PERCENTEQUAL     -> Format.fprintf fmt "PERCENTEQUAL"
     | AMPEREQUAL       -> Format.fprintf fmt "AMPEREQUAL"
     | VBAREQUAL        -> Format.fprintf fmt "VBAREQUAL"
     | CIRCUMFLEXEQUAL  -> Format.fprintf fmt "CIRCUMFLEXEQUAL"
     | LEFTSHIFTEQUAL   -> Format.fprintf fmt "LEFTSHIFTEQUAL"
     | RIGHTSHIFTEQUAL  -> Format.fprintf fmt "RIGHTSHIFTEQUAL"
     | DOUBLESTAREQUAL  -> Format.fprintf fmt "DOUBLESTAREQUAL"
     | DOUBLESLASH      -> Format.fprintf fmt "DOUBLESLASH"
     | DOUBLESLASHEQUAL -> Format.fprintf fmt "DOUBLESLASHEQUAL"
     | AT               -> Format.fprintf fmt "AT"
     | ATEQUAL          -> Format.fprintf fmt "ATEQUAL"
     | RARROW           -> Format.fprintf fmt "RARROW"
     | ELLIPSIS         -> Format.fprintf fmt "ELLIPSIS"
     | OP               -> Format.fprintf fmt "OP"
     | ERRORTOKEN       -> Format.fprintf fmt "ERRORTOKEN"
     | COMMENT          -> Format.fprintf fmt "COMMENT"
     | NL               -> Format.fprintf fmt "NL"
     | ENCODING         -> Format.fprintf fmt "ENCODING"
     | N_TOKENS         -> Format.fprintf fmt "N_TOKENS"
     | KW_and           -> Format.fprintf fmt "and"
     | KW_as            -> Format.fprintf fmt "as"
     | KW_assert        -> Format.fprintf fmt "assert"
     | KW_async         -> Format.fprintf fmt "async"
     | KW_await         -> Format.fprintf fmt "await"
     | KW_break         -> Format.fprintf fmt "break"
     | KW_class         -> Format.fprintf fmt "class"
     | KW_continue      -> Format.fprintf fmt "continue"
     | KW_def           -> Format.fprintf fmt "def"
     | KW_del           -> Format.fprintf fmt "del"
     | KW_elif          -> Format.fprintf fmt "elif"
     | KW_else          -> Format.fprintf fmt "else"
     | KW_except        -> Format.fprintf fmt "except"
     | KW_False         -> Format.fprintf fmt "False"
     | KW_finally       -> Format.fprintf fmt "finally"
     | KW_for           -> Format.fprintf fmt "for"
     | KW_from          -> Format.fprintf fmt "from"
     | KW_global        -> Format.fprintf fmt "global"
     | KW_if            -> Format.fprintf fmt "if"
     | KW_import        -> Format.fprintf fmt "import"
     | KW_in            -> Format.fprintf fmt "in"
     | KW_is            -> Format.fprintf fmt "is"
     | KW_lambda        -> Format.fprintf fmt "lambda"
     | KW_None          -> Format.fprintf fmt "None"
     | KW_nonlocal      -> Format.fprintf fmt "nonlocal"
     | KW_not           -> Format.fprintf fmt "not"
     | KW_or            -> Format.fprintf fmt "or"
     | KW_pass          -> Format.fprintf fmt "pass"
     | KW_raise         -> Format.fprintf fmt "raise"
     | KW_return        -> Format.fprintf fmt "return"
     | KW_True          -> Format.fprintf fmt "True"
     | KW_try           -> Format.fprintf fmt "try"
     | KW_while         -> Format.fprintf fmt "while"
     | KW_with          -> Format.fprintf fmt "with"
     | KW_yield         -> Format.fprintf fmt "yield"

  let compare : type a b.a t -> b t -> (a, b) Asp.Types.cmp =
    fun x y -> let open Asp.Types in
    match x, y with
    | ENDMARKER        , ENDMARKER        -> Eql
    | NAME             , NAME             -> Eql
    | NUMBER           , NUMBER           -> Eql
    | STRING           , STRING           -> Eql
    | NEWLINE          , NEWLINE          -> Eql
    | INDENT           , INDENT           -> Eql
    | DEDENT           , DEDENT           -> Eql
    | LPAR             , LPAR             -> Eql
    | RPAR             , RPAR             -> Eql
    | LSQB             , LSQB             -> Eql
    | RSQB             , RSQB             -> Eql
    | COLON            , COLON            -> Eql
    | COMMA            , COMMA            -> Eql
    | SEMI             , SEMI             -> Eql
    | PLUS             , PLUS             -> Eql
    | MINUS            , MINUS            -> Eql
    | STAR             , STAR             -> Eql
    | SLASH            , SLASH            -> Eql
    | VBAR             , VBAR             -> Eql
    | AMPER            , AMPER            -> Eql
    | LESS             , LESS             -> Eql
    | GREATER          , GREATER          -> Eql
    | EQUAL            , EQUAL            -> Eql
    | DOT              , DOT              -> Eql
    | PERCENT          , PERCENT          -> Eql
    | LBRACE           , LBRACE           -> Eql
    | RBRACE           , RBRACE           -> Eql
    | EQEQUAL          , EQEQUAL          -> Eql
    | NOTEQUAL         , NOTEQUAL         -> Eql
    | LESSEQUAL        , LESSEQUAL        -> Eql
    | GREATEREQUAL     , GREATEREQUAL     -> Eql
    | TILDE            , TILDE            -> Eql
    | CIRCUMFLEX       , CIRCUMFLEX       -> Eql
    | LEFTSHIFT        , LEFTSHIFT        -> Eql
    | RIGHTSHIFT       , RIGHTSHIFT       -> Eql
    | DOUBLESTAR       , DOUBLESTAR       -> Eql
    | PLUSEQUAL        , PLUSEQUAL        -> Eql
    | MINEQUAL         , MINEQUAL         -> Eql
    | STAREQUAL        , STAREQUAL        -> Eql
    | SLASHEQUAL       , SLASHEQUAL       -> Eql
    | PERCENTEQUAL     , PERCENTEQUAL     -> Eql
    | AMPEREQUAL       , AMPEREQUAL       -> Eql
    | VBAREQUAL        , VBAREQUAL        -> Eql
    | CIRCUMFLEXEQUAL  , CIRCUMFLEXEQUAL  -> Eql
    | LEFTSHIFTEQUAL   , LEFTSHIFTEQUAL   -> Eql
    | RIGHTSHIFTEQUAL  , RIGHTSHIFTEQUAL  -> Eql
    | DOUBLESTAREQUAL  , DOUBLESTAREQUAL  -> Eql
    | DOUBLESLASH      , DOUBLESLASH      -> Eql
    | DOUBLESLASHEQUAL , DOUBLESLASHEQUAL -> Eql
    | AT               , AT               -> Eql
    | ATEQUAL          , ATEQUAL          -> Eql
    | RARROW           , RARROW           -> Eql
    | ELLIPSIS         , ELLIPSIS         -> Eql
    | OP               , OP               -> Eql
    | ERRORTOKEN       , ERRORTOKEN       -> Eql
    | COMMENT          , COMMENT          -> Eql
    | NL               , NL               -> Eql
    | ENCODING         , ENCODING         -> Eql
    | N_TOKENS         , N_TOKENS         -> Eql

    | KW_and           , KW_and           -> Eql
    | KW_as            , KW_as            -> Eql
    | KW_assert        , KW_assert        -> Eql
    | KW_async         , KW_async         -> Eql
    | KW_await         , KW_await         -> Eql
    | KW_break         , KW_break         -> Eql
    | KW_class         , KW_class         -> Eql
    | KW_continue      , KW_continue      -> Eql
    | KW_def           , KW_def           -> Eql
    | KW_del           , KW_del           -> Eql
    | KW_elif          , KW_elif          -> Eql
    | KW_else          , KW_else          -> Eql
    | KW_except        , KW_except        -> Eql
    | KW_False         , KW_False         -> Eql
    | KW_finally       , KW_finally       -> Eql
    | KW_for           , KW_for           -> Eql
    | KW_from          , KW_from          -> Eql
    | KW_global        , KW_global        -> Eql
    | KW_if            , KW_if            -> Eql
    | KW_import        , KW_import        -> Eql
    | KW_in            , KW_in            -> Eql
    | KW_is            , KW_is            -> Eql
    | KW_lambda        , KW_lambda        -> Eql
    | KW_None          , KW_None          -> Eql
    | KW_nonlocal      , KW_nonlocal      -> Eql
    | KW_not           , KW_not           -> Eql
    | KW_or            , KW_or            -> Eql
    | KW_pass          , KW_pass          -> Eql
    | KW_raise         , KW_raise         -> Eql
    | KW_return        , KW_return        -> Eql
    | KW_True          , KW_True          -> Eql
    | KW_try           , KW_try           -> Eql
    | KW_while         , KW_while         -> Eql
    | KW_with          , KW_with          -> Eql
    | KW_yield         , KW_yield         -> Eql
    | x                , y                ->
       if Obj.repr x < Obj.repr y then Leq else Geq
end

type utag = U : _ tag -> utag [@@unboxed]

type t = T : 'a tag * 'a -> t

(* let to_int : utag -> int = function
     | U ENDMARKER -> 0
     | U NAME      -> 1
     | U NUMBER    -> 2
     | _ ...
*)
(* The following representation-reinterpreting coercion has the same
   behaviour as the safe definition above *)
let to_int : utag -> int = Obj.magic

let tag : t -> utag = function (T (tag,_)) -> U tag

module Ord =
struct
  type t = utag
  let compare l r = Pervasives.compare (to_int l) (to_int r)
end

module TagSet = Set.Make(Ord)
let all = TagSet.of_list
  [U ENDMARKER; U NAME; U NUMBER; U STRING; U NEWLINE; U INDENT; U DEDENT;
   U LPAR; U RPAR; U LSQB; U RSQB; U COLON; U COMMA; U SEMI; U PLUS;
   U MINUS; U STAR; U SLASH; U VBAR; U AMPER; U LESS; U GREATER; U EQUAL;
   U DOT; U PERCENT; U LBRACE; U RBRACE; U EQEQUAL; U NOTEQUAL;
   U LESSEQUAL; U GREATEREQUAL; U TILDE; U CIRCUMFLEX; U LEFTSHIFT;
   U RIGHTSHIFT; U DOUBLESTAR; U PLUSEQUAL; U MINEQUAL; U STAREQUAL;
   U SLASHEQUAL; U PERCENTEQUAL; U AMPEREQUAL; U VBAREQUAL;
   U CIRCUMFLEXEQUAL; U LEFTSHIFTEQUAL; U RIGHTSHIFTEQUAL;
   U DOUBLESTAREQUAL; U DOUBLESLASH; U DOUBLESLASHEQUAL; U AT; U ATEQUAL;
   U RARROW; U ELLIPSIS; U OP; U ERRORTOKEN; U COMMENT; U NL; U ENCODING;
   U N_TOKENS;

   U KW_and; U KW_as; U KW_assert; U KW_async; U KW_await; U KW_break;
   U KW_class; U KW_continue; U KW_def; U KW_del; U KW_elif; U KW_else;
   U KW_except; U KW_False; U KW_finally; U KW_for; U KW_from; U KW_global;
   U KW_if; U KW_import; U KW_in; U KW_is; U KW_lambda; U KW_None; U KW_nonlocal;
   U KW_not; U KW_or; U KW_pass; U KW_raise; U KW_return; U KW_True; U KW_try;
   U KW_while; U KW_with; U KW_yield ]

let inj t = U t

let match_ : type a b. t -> a tag -> (a -> b) -> (unit -> b) -> b =
  fun (T (tag, v)) tag' yes no ->
  if U tag = U tag' then yes (Obj.magic v) else no ()

let matchlist_ : type a b. t -> a tag list -> (a -> b) -> (unit -> b) -> b =
  fun (T (tag, v)) tags yes no ->
  if List.mem (Obj.magic tag) tags then yes (Obj.magic v) else no ()
