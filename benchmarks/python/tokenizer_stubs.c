#include <assert.h>
#include <string.h>

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

static value string_of_start_and_end(const char *start, const char *end)
{
  value v = caml_alloc_string(end - start);
  memcpy(String_val(v), start, end - start);
  return v;
}

value tokenizer_get(value s, value sref)
{
  CAMLparam2(s, sref);
  struct tok_state *state = tok_state_custom_val(s);
  char *start = NULL, *end = NULL;
  switch (PyTokenizer_Get(state, &start, &end)) {
  case ENDMARKER        : CAMLreturn(Val_int(ENDMARKER));
  case NAME             : Field(sref, 0) = string_of_start_and_end(start, end);
                          CAMLreturn(Val_int(NAME));
  case NUMBER           : Field(sref, 0) = string_of_start_and_end(start, end);
                          CAMLreturn(Val_int(NUMBER));
  case STRING           : Field(sref, 0) = string_of_start_and_end(start, end);
                          CAMLreturn(Val_int(STRING));
  case NEWLINE          : CAMLreturn(Val_int(NEWLINE));
  case INDENT           : CAMLreturn(Val_int(INDENT));
  case DEDENT           : CAMLreturn(Val_int(DEDENT));
  case LPAR             : CAMLreturn(Val_int(LPAR));
  case RPAR             : CAMLreturn(Val_int(RPAR));
  case LSQB             : CAMLreturn(Val_int(LSQB));
  case RSQB             : CAMLreturn(Val_int(RSQB));
  case COLON            : CAMLreturn(Val_int(COLON));
  case COMMA            : CAMLreturn(Val_int(COMMA));
  case SEMI             : CAMLreturn(Val_int(SEMI));
  case PLUS             : CAMLreturn(Val_int(PLUS));
  case MINUS            : CAMLreturn(Val_int(MINUS));
  case STAR             : CAMLreturn(Val_int(STAR));
  case SLASH            : CAMLreturn(Val_int(SLASH));
  case VBAR             : CAMLreturn(Val_int(VBAR));
  case AMPER            : CAMLreturn(Val_int(AMPER));
  case LESS             : CAMLreturn(Val_int(LESS));
  case GREATER          : CAMLreturn(Val_int(GREATER));
  case EQUAL            : CAMLreturn(Val_int(EQUAL));
  case DOT              : CAMLreturn(Val_int(DOT));
  case PERCENT          : CAMLreturn(Val_int(PERCENT));
  case LBRACE           : CAMLreturn(Val_int(LBRACE));
  case RBRACE           : CAMLreturn(Val_int(RBRACE));
  case EQEQUAL          : CAMLreturn(Val_int(EQEQUAL));
  case NOTEQUAL         : CAMLreturn(Val_int(NOTEQUAL));
  case LESSEQUAL        : CAMLreturn(Val_int(LESSEQUAL));
  case GREATEREQUAL     : CAMLreturn(Val_int(GREATEREQUAL));
  case TILDE            : CAMLreturn(Val_int(TILDE));
  case CIRCUMFLEX       : CAMLreturn(Val_int(CIRCUMFLEX));
  case LEFTSHIFT        : CAMLreturn(Val_int(LEFTSHIFT));
  case RIGHTSHIFT       : CAMLreturn(Val_int(RIGHTSHIFT));
  case DOUBLESTAR       : CAMLreturn(Val_int(DOUBLESTAR));
  case PLUSEQUAL        : CAMLreturn(Val_int(PLUSEQUAL));
  case MINEQUAL         : CAMLreturn(Val_int(MINEQUAL));
  case STAREQUAL        : CAMLreturn(Val_int(STAREQUAL));
  case SLASHEQUAL       : CAMLreturn(Val_int(SLASHEQUAL));
  case PERCENTEQUAL     : CAMLreturn(Val_int(PERCENTEQUAL));
  case AMPEREQUAL       : CAMLreturn(Val_int(AMPEREQUAL));
  case VBAREQUAL        : CAMLreturn(Val_int(VBAREQUAL));
  case CIRCUMFLEXEQUAL  : CAMLreturn(Val_int(CIRCUMFLEXEQUAL));
  case LEFTSHIFTEQUAL   : CAMLreturn(Val_int(LEFTSHIFTEQUAL));
  case RIGHTSHIFTEQUAL  : CAMLreturn(Val_int(RIGHTSHIFTEQUAL));
  case DOUBLESTAREQUAL  : CAMLreturn(Val_int(DOUBLESTAREQUAL));
  case DOUBLESLASH      : CAMLreturn(Val_int(DOUBLESLASH));
  case DOUBLESLASHEQUAL : CAMLreturn(Val_int(DOUBLESLASHEQUAL));
  case AT               : CAMLreturn(Val_int(AT));
  case ATEQUAL          : CAMLreturn(Val_int(ATEQUAL));
  case RARROW           : CAMLreturn(Val_int(RARROW));
  case ELLIPSIS         : CAMLreturn(Val_int(ELLIPSIS));
  case OP               : Field(sref, 0) = string_of_start_and_end(start, end);
                          CAMLreturn(Val_int(OP));
  case ERRORTOKEN       : caml_failwith("Tokenizing error");
  default               : caml_failwith("Tokenizer: internal error");
  }
}
