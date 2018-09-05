## asp: algebraic, staged parsing

`asp` is a typed, algebraic parser combinator library with some unusual features:

  * `asp` parsers are checked using an internal type system before they are
    run to ensure that the grammars they describe are unambiguous and
    free from left-recursion.

    These constraints ensure that the input can be parsed in linear
    time without backtracking and with a single token of lookahead.

  * `asp` uses multi-stage programming to achieve much higher
    performance than most combinator libraries.  Parsers constructed
    with `asp` can be compiled at runtime to efficient code that
    typically outperforms parsers written with the standard OCaml
    parser-generator `ocamlyacc`
    
The blog post [A typed, algebraic approach to parsing][blog-post]
gives a more complete introduction.  The [draft paper][paper] of the
same name has many more details.

## Installation

1. Install the [BER MetaOCaml][ber-metaocaml] compiler using [OPAM][opam]:

   ```
   opam switch 4.04.0+BER
   eval $(opam config env)
   ```

2. Pin the [`letrec` package][letrec]:

   ```
   opam pin add letrec https://github.com/yallop/metaocaml-letrec.git
   ```

3. Pin the `asp` package:

   ```
   opam pin add asp https://github.com/yallop/ocaml-asp.git
   ```


## Running the benchmarks

Clone the repository and type `make bench`.

[blog-post]: http://semantic-domain.blogspot.com/2018/07/a-typed-algebraic-approach-to-parsing.html
[paper]: http://www.cl.cam.ac.uk/~nk480/parsing.pdf
[opam]: https://opam.ocaml.org/
[letrec]: https://github.com/yallop/metaocaml-letrec
[ber-metaocaml]: http://okmij.org/ftp/ML/MetaOCaml.html
