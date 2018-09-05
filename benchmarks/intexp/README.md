## Expression langauge benchmark

#### Lexical syntax

    token ::= keyword
              x | y | z | ...
              int
              operator
              (
              )
    keyword ::= let | in | if | then | else
    operator ::= + | - | * | =
    
#### Syntax

    exp ::=
        x
        let x = exp in exp
        int
        exp operator exp
        if exp then exp else exp
        ( exp )

(with appropriate precedence for operators)

#### Benchmark

Parse and evaluate expressions in the language, returning a single
integer.
