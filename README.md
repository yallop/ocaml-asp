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
    
The following paper has many more details:

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;A typed, algebraic approach to parsing ([pdf][paper])  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Neelakantan R. Krishnaswami and Jeremy Yallop  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;PLDI 2019  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(received Distinguished Paper Award &amp; Distinguished Artifact Award)

## Trying out asp

The [pldi-artifact](pldi-artifact) directory contains instructions for running the artifact, which can be used to try out the library and reproduce the benchmarks without installing the software.

## Installation

1. Install the [BER MetaOCaml][ber-metaocaml] compiler using [OPAM][opam]:

   ```
   opam switch 4.07.1+BER
   eval $(opam env)
   ```

2. Add the [metaocaml-opam][metaocaml-opam] repository:

   ```
   opam remote add metaocaml git+https://github.com/metaocaml/metaocaml-opam.git
   ```

3. Install the `asp` package:

   ```
   opam install asp
   ```


## Running the benchmarks

Clone the repository and type `make bench`.

[paper]: https://www.cl.cam.ac.uk/~jdy22/papers/a-typed-algebraic-approach-to-parsing.pdf
[opam]: https://opam.ocaml.org/
[letrec]: https://github.com/yallop/metaocaml-letrec
[ber-metaocaml]: http://okmij.org/ftp/ML/MetaOCaml.html
[metaocaml-opam]: https://github.com/metaocaml/metaocaml-opam/
