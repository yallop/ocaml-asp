type t

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

type utag = U : _ tag -> utag [@@unboxed]

external from_string : string -> t             = "tokenizer_from_string"
external from_utf8   : string -> t             = "tokenizer_from_utf8"
external get         : t -> string ref -> utag = "tokenizer_get"

type token = Tok : 'a tag * 'a -> token

let to_stream : t -> token Stream.t =
  fun t ->
  let s = ref "" in
  Stream.from @@ fun _ ->
  match get t s with
  | U ENDMARKER -> None
  | U NAME      -> Some (Tok (NAME, !s))
  | U NUMBER    -> Some (Tok (NUMBER, !s))
  | U STRING    -> Some (Tok (STRING, !s))
  | U OP        -> Some (Tok (OP, !s))
  | U tag       -> Some (Tok (tag, Obj.magic ()))
