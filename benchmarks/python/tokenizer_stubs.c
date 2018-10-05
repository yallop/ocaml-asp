#include <assert.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#include "token.h"
#include "tokenizer.h"

#define tok_state_custom_val(V) (*(struct tok_state **)(Data_custom_val(V)))

static void finalize_tok_state(value v)
{
  PyTokenizer_Free(tok_state_custom_val(v));
}

static struct custom_operations tokenizer_ops = {
  "pytokenizer_tok_state",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default
};

value tokenizer_from_string(value s)
{
  CAMLparam1(s);
  struct tok_state *state = PyTokenizer_FromString(String_val(s), 0);
  CAMLlocal1(v);
  v = caml_alloc_custom(&tokenizer_ops, sizeof(struct tok_state *), 0, 1);
  tok_state_custom_val(v) = state;
  CAMLreturn(v);
}

value tokenizer_from_utf8(value s)
{
  CAMLparam1(s);
  struct tok_state *state = PyTokenizer_FromUTF8(String_val(s), 0);
  CAMLlocal1(v);
  v = caml_alloc_custom(&tokenizer_ops, sizeof(struct tok_state *), 0, 1);
  tok_state_custom_val(v) = state;
  CAMLreturn(v);
}

value tokenizer_get(value s)
{
  CAMLparam1(s);
  struct tok_state *state = tok_state_custom_val(s);
  char *start = NULL, *end = NULL;
  switch (PyTokenizer_Get(state, &start, &end)) {
  case ENDMARKER        : puts("ENDMARKER"); CAMLreturn(Val_unit);
  case NAME             : puts("NAME"); CAMLreturn(Val_unit);
  case NUMBER           : puts("NUMBER"); CAMLreturn(Val_unit);
  case STRING           : puts("STRING"); CAMLreturn(Val_unit);
  case NEWLINE          : puts("NEWLINE"); CAMLreturn(Val_unit);
  case INDENT           : puts("INDENT"); CAMLreturn(Val_unit);
  case DEDENT           : puts("DEDENT"); CAMLreturn(Val_unit);
  case LPAR             : puts("LPAR"); CAMLreturn(Val_unit);
  case RPAR             : puts("RPAR"); CAMLreturn(Val_unit);
  case LSQB             : puts("LSQB"); CAMLreturn(Val_unit);
  case RSQB             : puts("RSQB"); CAMLreturn(Val_unit);
  case COLON            : puts("COLON"); CAMLreturn(Val_unit);
  case COMMA            : puts("COMMA"); CAMLreturn(Val_unit);
  case SEMI             : puts("SEMI"); CAMLreturn(Val_unit);
  case PLUS             : puts("PLUS"); CAMLreturn(Val_unit);
  case MINUS            : puts("MINUS"); CAMLreturn(Val_unit);
  case STAR             : puts("STAR"); CAMLreturn(Val_unit);
  case SLASH            : puts("SLASH"); CAMLreturn(Val_unit);
  case VBAR             : puts("VBAR"); CAMLreturn(Val_unit);
  case AMPER            : puts("AMPER"); CAMLreturn(Val_unit);
  case LESS             : puts("LESS"); CAMLreturn(Val_unit);
  case GREATER          : puts("GREATER"); CAMLreturn(Val_unit);
  case EQUAL            : puts("EQUAL"); CAMLreturn(Val_unit);
  case DOT              : puts("DOT"); CAMLreturn(Val_unit);
  case PERCENT          : puts("PERCENT"); CAMLreturn(Val_unit);
  case LBRACE           : puts("LBRACE"); CAMLreturn(Val_unit);
  case RBRACE           : puts("RBRACE"); CAMLreturn(Val_unit);
  case EQEQUAL          : puts("EQEQUAL"); CAMLreturn(Val_unit);
  case NOTEQUAL         : puts("NOTEQUAL"); CAMLreturn(Val_unit);
  case LESSEQUAL        : puts("LESSEQUAL"); CAMLreturn(Val_unit);
  case GREATEREQUAL     : puts("GREATEREQUAL"); CAMLreturn(Val_unit);
  case TILDE            : puts("TILDE"); CAMLreturn(Val_unit);
  case CIRCUMFLEX       : puts("CIRCUMFLEX"); CAMLreturn(Val_unit);
  case LEFTSHIFT        : puts("LEFTSHIFT"); CAMLreturn(Val_unit);
  case RIGHTSHIFT       : puts("RIGHTSHIFT"); CAMLreturn(Val_unit);
  case DOUBLESTAR       : puts("DOUBLESTAR"); CAMLreturn(Val_unit);
  case PLUSEQUAL        : puts("PLUSEQUAL"); CAMLreturn(Val_unit);
  case MINEQUAL         : puts("MINEQUAL"); CAMLreturn(Val_unit);
  case STAREQUAL        : puts("STAREQUAL"); CAMLreturn(Val_unit);
  case SLASHEQUAL       : puts("SLASHEQUAL"); CAMLreturn(Val_unit);
  case PERCENTEQUAL     : puts("PERCENTEQUAL"); CAMLreturn(Val_unit);
  case AMPEREQUAL       : puts("AMPEREQUAL"); CAMLreturn(Val_unit);
  case VBAREQUAL        : puts("VBAREQUAL"); CAMLreturn(Val_unit);
  case CIRCUMFLEXEQUAL  : puts("CIRCUMFLEXEQUAL"); CAMLreturn(Val_unit);
  case LEFTSHIFTEQUAL   : puts("LEFTSHIFTEQUAL"); CAMLreturn(Val_unit);
  case RIGHTSHIFTEQUAL  : puts("RIGHTSHIFTEQUAL"); CAMLreturn(Val_unit);
  case DOUBLESTAREQUAL  : puts("DOUBLESTAREQUAL"); CAMLreturn(Val_unit);
  case DOUBLESLASH      : puts("DOUBLESLASH"); CAMLreturn(Val_unit);
  case DOUBLESLASHEQUAL : puts("DOUBLESLASHEQUAL"); CAMLreturn(Val_unit);
  case AT               : puts("AT"); CAMLreturn(Val_unit);
  case ATEQUAL          : puts("ATEQUAL"); CAMLreturn(Val_unit);
  case RARROW           : puts("RARROW"); CAMLreturn(Val_unit);
  case ELLIPSIS         : puts("ELLIPSIS"); CAMLreturn(Val_unit);
  case OP               : puts("OP"); CAMLreturn(Val_unit);
  case ERRORTOKEN       : caml_failwith("Tokenizing error");
  default               : caml_failwith("Tokenizer: internal error");
  }
}
