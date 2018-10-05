
/* Tokenizer implementation */

/* #include "Python.h" */
/* #include "pgenheaders.h" */

#include <ctype.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h> /* ssize_t */
#include <unistd.h> /* dup */

#include "tokenizer.h"
#include "errcode.h"

#define Py_CHARMASK(c) ((unsigned char)((c) & 0xff))

/* Alternate tab spacing */
#define ALTTABSIZE 1

#define is_potential_identifier_start(c) (\
              (c >= 'a' && c <= 'z')\
               || (c >= 'A' && c <= 'Z')\
               || c == '_'\
               || (c >= 128))

#define is_potential_identifier_char(c) (\
              (c >= 'a' && c <= 'z')\
               || (c >= 'A' && c <= 'Z')\
               || (c >= '0' && c <= '9')\
               || c == '_'\
               || (c >= 128))

extern char *PyOS_Readline(FILE *, FILE *, const char *);
/* Return malloc'ed string including trailing \n;
   empty malloc'ed string for EOF;
   NULL if interrupted */
char *
PyOS_Readline(FILE *sys_stdin, FILE *sys_stdout, const char *prompt)
{
    size_t n = 1000;
    char *p = (char *)malloc(n);
    char *q;
    if (p == NULL)
        return NULL;
    fprintf(stderr, "%s", prompt);
    q = fgets(p, n, sys_stdin);
    if (q == NULL) {
        *p = '\0';
        return p;
    }
    n = strlen(p);
    if (n > 0 && p[n-1] != '\n')
        p[n-1] = '\n';
    return (char *)realloc(p, n+1);
}

/* Don't ever change this -- it would break the portability of Python code */
#define TABSIZE 8

/* Forward */
static struct tok_state *tok_new(void);
static int tok_nextc(struct tok_state *tok);
static void tok_backup(struct tok_state *tok, int c);


/* Token names */

const char *_PyParser_TokenNames[] = {
    "ENDMARKER",
    "NAME",
    "NUMBER",
    "STRING",
    "NEWLINE",
    "INDENT",
    "DEDENT",
    "LPAR",
    "RPAR",
    "LSQB",
    "RSQB",
    "COLON",
    "COMMA",
    "SEMI",
    "PLUS",
    "MINUS",
    "STAR",
    "SLASH",
    "VBAR",
    "AMPER",
    "LESS",
    "GREATER",
    "EQUAL",
    "DOT",
    "PERCENT",
    "LBRACE",
    "RBRACE",
    "EQEQUAL",
    "NOTEQUAL",
    "LESSEQUAL",
    "GREATEREQUAL",
    "TILDE",
    "CIRCUMFLEX",
    "LEFTSHIFT",
    "RIGHTSHIFT",
    "DOUBLESTAR",
    "PLUSEQUAL",
    "MINEQUAL",
    "STAREQUAL",
    "SLASHEQUAL",
    "PERCENTEQUAL",
    "AMPEREQUAL",
    "VBAREQUAL",
    "CIRCUMFLEXEQUAL",
    "LEFTSHIFTEQUAL",
    "RIGHTSHIFTEQUAL",
    "DOUBLESTAREQUAL",
    "DOUBLESLASH",
    "DOUBLESLASHEQUAL",
    "AT",
    "ATEQUAL",
    "RARROW",
    "ELLIPSIS",
    /* This table must match the #defines in token.h! */
    "OP",
    "<ERRORTOKEN>",
    "COMMENT",
    "NL",
    "ENCODING",
    "<N_TOKENS>"
};


/* Create and initialize a new tok_state structure */

static struct tok_state *
tok_new(void)
{
    struct tok_state *tok = (struct tok_state *)malloc(
                                            sizeof(struct tok_state));
    if (tok == NULL)
        return NULL;
    tok->buf = tok->cur = tok->end = tok->inp = tok->start = NULL;
    tok->done = E_OK;
    tok->fp = NULL;
    tok->input = NULL;
    tok->tabsize = TABSIZE;
    tok->indent = 0;
    tok->indstack[0] = 0;

    tok->atbol = 1;
    tok->pendin = 0;
    tok->prompt = tok->nextprompt = NULL;
    tok->lineno = 0;
    tok->level = 0;
    tok->altindstack[0] = 0;
    tok->decoding_state = STATE_INIT;
    tok->decoding_erred = 0;
    tok->read_coding_spec = 0;
    tok->enc = NULL;
    tok->encoding = NULL;
    tok->cont_line = 0;

    return tok;
}

static char *
new_string(const char *s, ssize_t len, struct tok_state *tok)
{
    char* result = (char *)malloc(len + 1);
    if (!result) {
        tok->done = E_NOMEM;
        return NULL;
    }
    memcpy(result, s, len);
    result[len] = '\0';
    return result;
}

static char *
decoding_fgets(char *s, int size, struct tok_state *tok)
{
    return fgets(s, size, tok->fp);
}

static int
decoding_feof(struct tok_state *tok)
{
    return feof(tok->fp);
}

static char *
decode_str(const char *str, int exec_input, struct tok_state *tok)
{
    return new_string(str, strlen(str), tok);
}

/* Set up tokenizer for string */

struct tok_state *
PyTokenizer_FromString(const char *str, int exec_input)
{
    struct tok_state *tok = tok_new();
    if (tok == NULL)
        return NULL;
    str = decode_str(str, exec_input, tok);
    if (str == NULL) {
        PyTokenizer_Free(tok);
        return NULL;
    }

    /* XXX: constify members. */
    tok->buf = tok->cur = tok->end = tok->inp = (char*)str;
    return tok;
}

struct tok_state *
PyTokenizer_FromUTF8(const char *str, int exec_input)
{
    struct tok_state *tok = tok_new();
    if (tok == NULL)
        return NULL;
    if (str == NULL) {
        PyTokenizer_Free(tok);
        return NULL;
    }
    tok->decoding_state = STATE_RAW;
    tok->read_coding_spec = 1;
    tok->enc = NULL;
    tok->str = str;
    tok->encoding = (char *)malloc(6);
    if (!tok->encoding) {
        PyTokenizer_Free(tok);
        return NULL;
    }
    strcpy(tok->encoding, "utf-8");

    /* XXX: constify members. */
    tok->buf = tok->cur = tok->end = tok->inp = (char*)str;
    return tok;
}

/* Set up tokenizer for file */

struct tok_state *
PyTokenizer_FromFile(FILE *fp, const char* enc,
                     const char *ps1, const char *ps2)
{
    struct tok_state *tok = tok_new();
    if (tok == NULL)
        return NULL;
    if ((tok->buf = (char *)malloc(BUFSIZ)) == NULL) {
        PyTokenizer_Free(tok);
        return NULL;
    }
    tok->cur = tok->inp = tok->buf;
    tok->end = tok->buf + BUFSIZ;
    tok->fp = fp;
    tok->prompt = ps1;
    tok->nextprompt = ps2;
    if (enc != NULL) {
        /* Must copy encoding declaration since it
           gets copied into the parse tree. */
        tok->encoding = malloc(strlen(enc)+1);
        if (!tok->encoding) {
            PyTokenizer_Free(tok);
            return NULL;
        }
        strcpy(tok->encoding, enc);
        tok->decoding_state = STATE_NORMAL;
    }
    return tok;
}


/* Free a tok_state structure */

void
PyTokenizer_Free(struct tok_state *tok)
{
    if (tok->encoding != NULL)
        free(tok->encoding);
    if (tok->fp != NULL && tok->buf != NULL)
        free(tok->buf);
    if (tok->input)
        free((char *)tok->input);
    free(tok);
}

/* Get next char, updating state; error code goes into tok->done */

static int
tok_nextc(struct tok_state *tok)
{
    for (;;) {
        if (tok->cur != tok->inp) {
            return Py_CHARMASK(*tok->cur++); /* Fast path */
        }
        if (tok->done != E_OK)
            return EOF;
        if (tok->fp == NULL) {
            char *end = strchr(tok->inp, '\n');
            if (end != NULL)
                end++;
            else {
                end = strchr(tok->inp, '\0');
                if (end == tok->inp) {
                    tok->done = E_EOF;
                    return EOF;
                }
            }
            if (tok->start == NULL)
                tok->buf = tok->cur;
            tok->line_start = tok->cur;
            tok->lineno++;
            tok->inp = end;
            return Py_CHARMASK(*tok->cur++);
        }
        if (tok->prompt != NULL) {
            char *newtok = PyOS_Readline(stdin, stdout, tok->prompt);
            if (tok->nextprompt != NULL)
                tok->prompt = tok->nextprompt;
            if (newtok == NULL)
                tok->done = E_INTR;
            else if (*newtok == '\0') {
                free(newtok);
                tok->done = E_EOF;
            }
            else if (tok->start != NULL) {
                size_t start = tok->start - tok->buf;
                size_t oldlen = tok->cur - tok->buf;
                size_t newlen = oldlen + strlen(newtok);
                char *buf = tok->buf;
                buf = (char *)realloc(buf, newlen+1);
                tok->lineno++;
                if (buf == NULL) {
                    free(tok->buf);
                    tok->buf = NULL;
                    free(newtok);
                    tok->done = E_NOMEM;
                    return EOF;
                }
                tok->buf = buf;
                tok->cur = tok->buf + oldlen;
                tok->line_start = tok->cur;
                strcpy(tok->buf + oldlen, newtok);
                free(newtok);
                tok->inp = tok->buf + newlen;
                tok->end = tok->inp + 1;
                tok->start = tok->buf + start;
            }
            else {
                tok->lineno++;
                if (tok->buf != NULL)
                    free(tok->buf);
                tok->buf = newtok;
                tok->cur = tok->buf;
                tok->line_start = tok->buf;
                tok->inp = strchr(tok->buf, '\0');
                tok->end = tok->inp + 1;
            }
        }
        else {
            int done = 0;
            ssize_t cur = 0;
            char *pt;
            if (tok->start == NULL) {
                if (tok->buf == NULL) {
                    tok->buf = (char *)
                        malloc(BUFSIZ);
                    if (tok->buf == NULL) {
                        tok->done = E_NOMEM;
                        return EOF;
                    }
                    tok->end = tok->buf + BUFSIZ;
                }
                if (decoding_fgets(tok->buf, (int)(tok->end - tok->buf),
                          tok) == NULL) {
                    if (!tok->decoding_erred)
                        tok->done = E_EOF;
                    done = 1;
                }
                else {
                    tok->done = E_OK;
                    tok->inp = strchr(tok->buf, '\0');
                    done = tok->inp == tok->buf || tok->inp[-1] == '\n';
                }
            }
            else {
                cur = tok->cur - tok->buf;
                if (decoding_feof(tok)) {
                    tok->done = E_EOF;
                    done = 1;
                }
                else
                    tok->done = E_OK;
            }
            tok->lineno++;
            /* Read until '\n' or EOF */
            while (!done) {
                ssize_t curstart = tok->start == NULL ? -1 :
                          tok->start - tok->buf;
                ssize_t curvalid = tok->inp - tok->buf;
                ssize_t newsize = curvalid + BUFSIZ;
                char *newbuf = tok->buf;
                newbuf = (char *)realloc(newbuf,
                                               newsize);
                if (newbuf == NULL) {
                    tok->done = E_NOMEM;
                    tok->cur = tok->inp;
                    return EOF;
                }
                tok->buf = newbuf;
                tok->cur = tok->buf + cur;
                tok->line_start = tok->cur;
                tok->inp = tok->buf + curvalid;
                tok->end = tok->buf + newsize;
                tok->start = curstart < 0 ? NULL :
                         tok->buf + curstart;
                if (decoding_fgets(tok->inp,
                               (int)(tok->end - tok->inp),
                               tok) == NULL) {
                    /* Break out early on decoding
                       errors, as tok->buf will be NULL
                     */
                    if (tok->decoding_erred)
                        return EOF;
                    /* Last line does not end in \n,
                       fake one */
                    strcpy(tok->inp, "\n");
                }
                tok->inp = strchr(tok->inp, '\0');
                done = tok->inp[-1] == '\n';
            }
            if (tok->buf != NULL) {
                tok->cur = tok->buf + cur;
                tok->line_start = tok->cur;
                /* replace "\r\n" with "\n" */
                /* For Mac leave the \r, giving a syntax error */
                pt = tok->inp - 2;
                if (pt >= tok->buf && *pt == '\r') {
                    *pt++ = '\n';
                    *pt = '\0';
                    tok->inp = pt;
                }
            }
        }
        if (tok->done != E_OK) {
            if (tok->prompt != NULL)
                /* PySys_WriteStderr("\n"); */
              fputc('\n', stderr);
            tok->cur = tok->inp;
            return EOF;
        }
    }
    /*NOTREACHED*/
}


/* Back-up one character */

static void
tok_backup(struct tok_state *tok, int c)
{
    if (c != EOF) {
        if (--tok->cur < tok->buf)
          assert (((void)"tok_backup: beginning of buffer", 0));
        if (*tok->cur != c)
            *tok->cur = c;
    }
}


/* Return the token corresponding to a single character */

int
PyToken_OneChar(int c)
{
    switch (c) {
    case '(':           return LPAR;
    case ')':           return RPAR;
    case '[':           return LSQB;
    case ']':           return RSQB;
    case ':':           return COLON;
    case ',':           return COMMA;
    case ';':           return SEMI;
    case '+':           return PLUS;
    case '-':           return MINUS;
    case '*':           return STAR;
    case '/':           return SLASH;
    case '|':           return VBAR;
    case '&':           return AMPER;
    case '<':           return LESS;
    case '>':           return GREATER;
    case '=':           return EQUAL;
    case '.':           return DOT;
    case '%':           return PERCENT;
    case '{':           return LBRACE;
    case '}':           return RBRACE;
    case '^':           return CIRCUMFLEX;
    case '~':           return TILDE;
    case '@':           return AT;
    default:            return OP;
    }
}


int
PyToken_TwoChars(int c1, int c2)
{
    switch (c1) {
    case '=':
        switch (c2) {
        case '=':               return EQEQUAL;
        }
        break;
    case '!':
        switch (c2) {
        case '=':               return NOTEQUAL;
        }
        break;
    case '<':
        switch (c2) {
        case '>':               return NOTEQUAL;
        case '=':               return LESSEQUAL;
        case '<':               return LEFTSHIFT;
        }
        break;
    case '>':
        switch (c2) {
        case '=':               return GREATEREQUAL;
        case '>':               return RIGHTSHIFT;
        }
        break;
    case '+':
        switch (c2) {
        case '=':               return PLUSEQUAL;
        }
        break;
    case '-':
        switch (c2) {
        case '=':               return MINEQUAL;
        case '>':               return RARROW;
        }
        break;
    case '*':
        switch (c2) {
        case '*':               return DOUBLESTAR;
        case '=':               return STAREQUAL;
        }
        break;
    case '/':
        switch (c2) {
        case '/':               return DOUBLESLASH;
        case '=':               return SLASHEQUAL;
        }
        break;
    case '|':
        switch (c2) {
        case '=':               return VBAREQUAL;
        }
        break;
    case '%':
        switch (c2) {
        case '=':               return PERCENTEQUAL;
        }
        break;
    case '&':
        switch (c2) {
        case '=':               return AMPEREQUAL;
        }
        break;
    case '^':
        switch (c2) {
        case '=':               return CIRCUMFLEXEQUAL;
        }
        break;
    case '@':
        switch (c2) {
        case '=':               return ATEQUAL;
        }
        break;
    }
    return OP;
}

int
PyToken_ThreeChars(int c1, int c2, int c3)
{
    switch (c1) {
    case '<':
        switch (c2) {
        case '<':
            switch (c3) {
            case '=':
                return LEFTSHIFTEQUAL;
            }
            break;
        }
        break;
    case '>':
        switch (c2) {
        case '>':
            switch (c3) {
            case '=':
                return RIGHTSHIFTEQUAL;
            }
            break;
        }
        break;
    case '*':
        switch (c2) {
        case '*':
            switch (c3) {
            case '=':
                return DOUBLESTAREQUAL;
            }
            break;
        }
        break;
    case '/':
        switch (c2) {
        case '/':
            switch (c3) {
            case '=':
                return DOUBLESLASHEQUAL;
            }
            break;
        }
        break;
    case '.':
        switch (c2) {
        case '.':
            switch (c3) {
            case '.':
                return ELLIPSIS;
            }
            break;
        }
        break;
    }
    return OP;
}

static int
syntaxerror(struct tok_state *tok, const char *format, ...)
{
    tok->done = E_TOKEN;
    return ERRORTOKEN;
}

static int
indenterror(struct tok_state *tok)
{
    tok->done = E_TABSPACE;
    tok->cur = tok->inp;
    return ERRORTOKEN;
}

#define verify_identifier(tok) 1

static int
tok_decimal_tail(struct tok_state *tok)
{
    int c;

    while (1) {
        do {
            c = tok_nextc(tok);
        } while (isdigit(c));
        if (c != '_') {
            break;
        }
        c = tok_nextc(tok);
        if (!isdigit(c)) {
            tok_backup(tok, c);
            syntaxerror(tok, "invalid decimal literal");
            return 0;
        }
    }
    return c;
}

/* Get next token, after space stripping etc. */

static int
tok_get(struct tok_state *tok, char **p_start, char **p_end)
{
    int c;
    int blankline, nonascii;

    *p_start = *p_end = NULL;
  nextline:
    tok->start = NULL;
    blankline = 0;

    /* Get indentation level */
    if (tok->atbol) {
        int col = 0;
        int altcol = 0;
        tok->atbol = 0;
        for (;;) {
            c = tok_nextc(tok);
            if (c == ' ') {
                col++, altcol++;
            }
            else if (c == '\t') {
                col = (col / tok->tabsize + 1) * tok->tabsize;
                altcol = (altcol / ALTTABSIZE + 1) * ALTTABSIZE;
            }
            else if (c == '\014')  {/* Control-L (formfeed) */
                col = altcol = 0; /* For Emacs users */
            }
            else {
                break;
            }
        }
        tok_backup(tok, c);
        if (c == '#' || c == '\n') {
            /* Lines with only whitespace and/or comments
               shouldn't affect the indentation and are
               not passed to the parser as NEWLINE tokens,
               except *totally* empty lines in interactive
               mode, which signal the end of a command group. */
            if (col == 0 && c == '\n' && tok->prompt != NULL) {
                blankline = 0; /* Let it through */
            }
            else {
                blankline = 1; /* Ignore completely */
            }
            /* We can't jump back right here since we still
               may need to skip to the end of a comment */
        }
        if (!blankline && tok->level == 0) {
            if (col == tok->indstack[tok->indent]) {
                /* No change */
                if (altcol != tok->altindstack[tok->indent]) {
                    return indenterror(tok);
                }
            }
            else if (col > tok->indstack[tok->indent]) {
                /* Indent -- always one */
                if (tok->indent+1 >= MAXINDENT) {
                    tok->done = E_TOODEEP;
                    tok->cur = tok->inp;
                    return ERRORTOKEN;
                }
                if (altcol <= tok->altindstack[tok->indent]) {
                    return indenterror(tok);
                }
                tok->pendin++;
                tok->indstack[++tok->indent] = col;
                tok->altindstack[tok->indent] = altcol;
            }
            else /* col < tok->indstack[tok->indent] */ {
                /* Dedent -- any number, must be consistent */
                while (tok->indent > 0 &&
                    col < tok->indstack[tok->indent]) {
                    tok->pendin--;
                    tok->indent--;
                }
                if (col != tok->indstack[tok->indent]) {
                    tok->done = E_DEDENT;
                    tok->cur = tok->inp;
                    return ERRORTOKEN;
                }
                if (altcol != tok->altindstack[tok->indent]) {
                    return indenterror(tok);
                }
            }
        }
    }

    tok->start = tok->cur;

    /* Return pending indents/dedents */
    if (tok->pendin != 0) {
        if (tok->pendin < 0) {
            tok->pendin++;
            return DEDENT;
        }
        else {
            tok->pendin--;
            return INDENT;
        }
    }

 again:
    tok->start = NULL;
    /* Skip spaces */
    do {
        c = tok_nextc(tok);
    } while (c == ' ' || c == '\t' || c == '\014');

    /* Set start of current token */
    tok->start = tok->cur - 1;

    /* Skip comment */
    if (c == '#') {
        while (c != EOF && c != '\n') {
            c = tok_nextc(tok);
        }
    }

    /* Check for EOF and errors now */
    if (c == EOF) {
        return tok->done == E_EOF ? ENDMARKER : ERRORTOKEN;
    }

    /* Identifier (most frequent token!) */
    nonascii = 0;
    if (is_potential_identifier_start(c)) {
        /* Process the various legal combinations of b"", r"", u"", and f"". */
        int saw_b = 0, saw_r = 0, saw_u = 0, saw_f = 0;
        while (1) {
            if (!(saw_b || saw_u || saw_f) && (c == 'b' || c == 'B'))
                saw_b = 1;
            /* Since this is a backwards compatibility support literal we don't
               want to support it in arbitrary order like byte literals. */
            else if (!(saw_b || saw_u || saw_r || saw_f)
                     && (c == 'u'|| c == 'U')) {
                saw_u = 1;
            }
            /* ur"" and ru"" are not supported */
            else if (!(saw_r || saw_u) && (c == 'r' || c == 'R')) {
                saw_r = 1;
            }
            else if (!(saw_f || saw_b || saw_u) && (c == 'f' || c == 'F')) {
                saw_f = 1;
            }
            else {
                break;
            }
            c = tok_nextc(tok);
            if (c == '"' || c == '\'') {
                goto letter_quote;
            }
        }
        while (is_potential_identifier_char(c)) {
            if (c >= 128) {
                nonascii = 1;
            }
            c = tok_nextc(tok);
        }
        tok_backup(tok, c);
        if (nonascii && !verify_identifier(tok)) {
            return ERRORTOKEN;
        }
        *p_start = tok->start;
        *p_end = tok->cur;

        return NAME;
    }

    /* Newline */
    if (c == '\n') {
        tok->atbol = 1;
        if (blankline || tok->level > 0) {
            goto nextline;
        }
        *p_start = tok->start;
        *p_end = tok->cur - 1; /* Leave '\n' out of the string */
        tok->cont_line = 0;
        return NEWLINE;
    }

    /* Period or number starting with period? */
    if (c == '.') {
        c = tok_nextc(tok);
        if (isdigit(c)) {
            goto fraction;
        } else if (c == '.') {
            c = tok_nextc(tok);
            if (c == '.') {
                *p_start = tok->start;
                *p_end = tok->cur;
                return ELLIPSIS;
            }
            else {
                tok_backup(tok, c);
            }
            tok_backup(tok, '.');
        }
        else {
            tok_backup(tok, c);
        }
        *p_start = tok->start;
        *p_end = tok->cur;
        return DOT;
    }

    /* Number */
    if (isdigit(c)) {
        if (c == '0') {
            /* Hex, octal or binary -- maybe. */
            c = tok_nextc(tok);
            if (c == 'x' || c == 'X') {
                /* Hex */
                c = tok_nextc(tok);
                do {
                    if (c == '_') {
                        c = tok_nextc(tok);
                    }
                    if (!isxdigit(c)) {
                        tok_backup(tok, c);
                        return syntaxerror(tok, "invalid hexadecimal literal");
                    }
                    do {
                        c = tok_nextc(tok);
                    } while (isxdigit(c));
                } while (c == '_');
            }
            else if (c == 'o' || c == 'O') {
                /* Octal */
                c = tok_nextc(tok);
                do {
                    if (c == '_') {
                        c = tok_nextc(tok);
                    }
                    if (c < '0' || c >= '8') {
                        tok_backup(tok, c);
                        if (isdigit(c)) {
                            return syntaxerror(tok,
                                    "invalid digit '%c' in octal literal", c);
                        }
                        else {
                            return syntaxerror(tok, "invalid octal literal");
                        }
                    }
                    do {
                        c = tok_nextc(tok);
                    } while ('0' <= c && c < '8');
                } while (c == '_');
                if (isdigit(c)) {
                    return syntaxerror(tok,
                            "invalid digit '%c' in octal literal", c);
                }
            }
            else if (c == 'b' || c == 'B') {
                /* Binary */
                c = tok_nextc(tok);
                do {
                    if (c == '_') {
                        c = tok_nextc(tok);
                    }
                    if (c != '0' && c != '1') {
                        tok_backup(tok, c);
                        if (isdigit(c)) {
                            return syntaxerror(tok,
                                    "invalid digit '%c' in binary literal", c);
                        }
                        else {
                            return syntaxerror(tok, "invalid binary literal");
                        }
                    }
                    do {
                        c = tok_nextc(tok);
                    } while (c == '0' || c == '1');
                } while (c == '_');
                if (isdigit(c)) {
                    return syntaxerror(tok,
                            "invalid digit '%c' in binary literal", c);
                }
            }
            else {
                int nonzero = 0;
                /* maybe old-style octal; c is first char of it */
                /* in any case, allow '0' as a literal */
                while (1) {
                    if (c == '_') {
                        c = tok_nextc(tok);
                        if (!isdigit(c)) {
                            tok_backup(tok, c);
                            return syntaxerror(tok, "invalid decimal literal");
                        }
                    }
                    if (c != '0') {
                        break;
                    }
                    c = tok_nextc(tok);
                }
                if (isdigit(c)) {
                    nonzero = 1;
                    c = tok_decimal_tail(tok);
                    if (c == 0) {
                        return ERRORTOKEN;
                    }
                }
                if (c == '.') {
                    c = tok_nextc(tok);
                    goto fraction;
                }
                else if (c == 'e' || c == 'E') {
                    goto exponent;
                }
                else if (c == 'j' || c == 'J') {
                    goto imaginary;
                }
                else if (nonzero) {
                    /* Old-style octal: now disallowed. */
                    tok_backup(tok, c);
                    return syntaxerror(tok,
                                       "leading zeros in decimal integer "
                                       "literals are not permitted; "
                                       "use an 0o prefix for octal integers");
                }
            }
        }
        else {
            /* Decimal */
            c = tok_decimal_tail(tok);
            if (c == 0) {
                return ERRORTOKEN;
            }
            {
                /* Accept floating point numbers. */
                if (c == '.') {
                    c = tok_nextc(tok);
        fraction:
                    /* Fraction */
                    if (isdigit(c)) {
                        c = tok_decimal_tail(tok);
                        if (c == 0) {
                            return ERRORTOKEN;
                        }
                    }
                }
                if (c == 'e' || c == 'E') {
                    int e;
                  exponent:
                    e = c;
                    /* Exponent part */
                    c = tok_nextc(tok);
                    if (c == '+' || c == '-') {
                        c = tok_nextc(tok);
                        if (!isdigit(c)) {
                            tok_backup(tok, c);
                            return syntaxerror(tok, "invalid decimal literal");
                        }
                    } else if (!isdigit(c)) {
                        tok_backup(tok, c);
                        tok_backup(tok, e);
                        *p_start = tok->start;
                        *p_end = tok->cur;
                        return NUMBER;
                    }
                    c = tok_decimal_tail(tok);
                    if (c == 0) {
                        return ERRORTOKEN;
                    }
                }
                if (c == 'j' || c == 'J') {
                    /* Imaginary part */
        imaginary:
                    c = tok_nextc(tok);
                }
            }
        }
        tok_backup(tok, c);
        *p_start = tok->start;
        *p_end = tok->cur;
        return NUMBER;
    }

  letter_quote:
    /* String */
    if (c == '\'' || c == '"') {
        int quote = c;
        int quote_size = 1;             /* 1 or 3 */
        int end_quote_size = 0;

        /* Find the quote size and start of string */
        c = tok_nextc(tok);
        if (c == quote) {
            c = tok_nextc(tok);
            if (c == quote) {
                quote_size = 3;
            }
            else {
                end_quote_size = 1;     /* empty string found */
            }
        }
        if (c != quote) {
            tok_backup(tok, c);
        }

        /* Get rest of string */
        while (end_quote_size != quote_size) {
            c = tok_nextc(tok);
            if (c == EOF) {
                if (quote_size == 3) {
                    tok->done = E_EOFS;
                }
                else {
                    tok->done = E_EOLS;
                }
                tok->cur = tok->inp;
                return ERRORTOKEN;
            }
            if (quote_size == 1 && c == '\n') {
                tok->done = E_EOLS;
                tok->cur = tok->inp;
                return ERRORTOKEN;
            }
            if (c == quote) {
                end_quote_size += 1;
            }
            else {
                end_quote_size = 0;
                if (c == '\\') {
                    tok_nextc(tok);  /* skip escaped char */
                }
            }
        }

        *p_start = tok->start;
        *p_end = tok->cur;
        return STRING;
    }

    /* Line continuation */
    if (c == '\\') {
        c = tok_nextc(tok);
        if (c != '\n') {
            tok->done = E_LINECONT;
            tok->cur = tok->inp;
            return ERRORTOKEN;
        }
        tok->cont_line = 1;
        goto again; /* Read next line */
    }

    /* Check for two-character token */
    {
        int c2 = tok_nextc(tok);
        int token = PyToken_TwoChars(c, c2);
        if (token != OP) {
            int c3 = tok_nextc(tok);
            int token3 = PyToken_ThreeChars(c, c2, c3);
            if (token3 != OP) {
                token = token3;
            }
            else {
                tok_backup(tok, c3);
            }
            *p_start = tok->start;
            *p_end = tok->cur;
            return token;
        }
        tok_backup(tok, c2);
    }

    /* Keep track of parentheses nesting level */
    switch (c) {
    case '(':
    case '[':
    case '{':
        tok->level++;
        break;
    case ')':
    case ']':
    case '}':
        tok->level--;
        break;
    }

    /* Punctuation character */
    *p_start = tok->start;
    *p_end = tok->cur;
    return PyToken_OneChar(c);
}

int
PyTokenizer_Get(struct tok_state *tok, char **p_start, char **p_end)
{
    int result = tok_get(tok, p_start, p_end);
    if (tok->decoding_erred) {
        result = ERRORTOKEN;
        tok->done = E_DECODE;
    }
    return result;
}

/* Get the encoding of a Python file. Check for the coding cookie and check if
   the file starts with a BOM.

   PyTokenizer_FindEncodingFilename() returns NULL when it can't find the
   encoding in the first or second line of the file (in which case the encoding
   should be assumed to be UTF-8).

   The char* returned is malloc'ed via malloc() and thus must be freed
   by the caller. */

char *
PyTokenizer_FindEncodingFilename(int fd, void *filename)
{
    struct tok_state *tok;
    FILE *fp;
    char *p_start =NULL , *p_end =NULL , *encoding = NULL;

    fd = dup(fd);
    if (fd < 0) {
        return NULL;
    }

    fp = fdopen(fd, "r");
    if (fp == NULL) {
        return NULL;
    }
    tok = PyTokenizer_FromFile(fp, NULL, NULL, NULL);
    if (tok == NULL) {
        fclose(fp);
        return NULL;
    }
    while (tok->lineno < 2 && tok->done == E_OK) {
        PyTokenizer_Get(tok, &p_start, &p_end);
    }
    fclose(fp);
    if (tok->encoding) {
        encoding = (char *)malloc(strlen(tok->encoding) + 1);
        if (encoding)
        strcpy(encoding, tok->encoding);
    }
    PyTokenizer_Free(tok);
    return encoding;
}

char *
PyTokenizer_FindEncoding(int fd)
{
    return PyTokenizer_FindEncodingFilename(fd, NULL);
}
