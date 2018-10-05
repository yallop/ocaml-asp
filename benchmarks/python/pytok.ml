type t

external from_string : string -> t = "tokenizer_from_string"
external from_utf8   : string -> t = "tokenizer_from_utf8"
external get         : t -> unit   = "tokenizer_get"
