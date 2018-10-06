type t

module T = Python_tokens_base

external from_string : string -> t             = "tokenizer_from_string"
external from_utf8   : string -> t             = "tokenizer_from_utf8"
external get         : t -> string ref -> T.utag = "tokenizer_get"

let to_stream : t -> T.t Stream.t =
  fun t ->
  let s = ref "" in
  Stream.from @@ fun _ ->
  match get t s with
  | T.U T.ENDMARKER -> None
  | T.U T.NAME      ->
     begin match !s with
     | "and"        -> Some (T.T (T.KW_and,      ()))
     | "as"         -> Some (T.T (T.KW_as,       ()))
     | "assert"     -> Some (T.T (T.KW_assert,   ()))
     | "async"      -> Some (T.T (T.KW_async,    ()))
     | "await"      -> Some (T.T (T.KW_await,    ()))
     | "break"      -> Some (T.T (T.KW_break,    ()))
     | "class"      -> Some (T.T (T.KW_class,    ()))
     | "continue"   -> Some (T.T (T.KW_continue, ()))
     | "def"        -> Some (T.T (T.KW_def,      ()))
     | "del"        -> Some (T.T (T.KW_del,      ()))
     | "elif"       -> Some (T.T (T.KW_elif,     ()))
     | "else"       -> Some (T.T (T.KW_else,     ()))
     | "except"     -> Some (T.T (T.KW_except,   ()))
     | "False"      -> Some (T.T (T.KW_False,    ()))
     | "finally"    -> Some (T.T (T.KW_finally,  ()))
     | "for"        -> Some (T.T (T.KW_for,      ()))
     | "from"       -> Some (T.T (T.KW_from,     ()))
     | "global"     -> Some (T.T (T.KW_global,   ()))
     | "if"         -> Some (T.T (T.KW_if,       ()))
     | "import"     -> Some (T.T (T.KW_import,   ()))
     | "in"         -> Some (T.T (T.KW_in,       ()))
     | "is"         -> Some (T.T (T.KW_is,       ()))
     | "lambda"     -> Some (T.T (T.KW_lambda,   ()))
     | "None"       -> Some (T.T (T.KW_None,     ()))
     | "nonlocal"   -> Some (T.T (T.KW_nonlocal, ()))
     | "not"        -> Some (T.T (T.KW_not,      ()))
     | "or"         -> Some (T.T (T.KW_or,       ()))
     | "pass"       -> Some (T.T (T.KW_pass,     ()))
     | "raise"      -> Some (T.T (T.KW_raise,    ()))
     | "return"     -> Some (T.T (T.KW_return,   ()))
     | "True"       -> Some (T.T (T.KW_True,     ()))
     | "try"        -> Some (T.T (T.KW_try,      ()))
     | "while"      -> Some (T.T (T.KW_while,    ()))
     | "with"       -> Some (T.T (T.KW_with,     ()))
     | "yield"      -> Some (T.T (T.KW_yield,    ()))
     | name         -> Some (T.T (T.NAME, name))
     end
  | T.U T.NUMBER    -> Some (T.T (T.NUMBER, !s))
  | T.U T.STRING    -> Some (T.T (T.STRING, !s))
  | T.U T.OP        -> Some (T.T (T.OP, !s))
  | T.U tag         -> Some (T.T (tag, Obj.magic ()))
