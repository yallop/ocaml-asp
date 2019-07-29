## Getting Started Guide

The artifact is a Docker image.  The source files are in this
directory.  The image may be downloaded from the following URL:

    https://drive.google.com/open?id=1bVp90a3CVpc7ghmSaCpCmn7vEJpmm5wK

We have tested it with Docker version 18.09.2, but we expect it to
work with any recent version of Docker.

The following steps may be followed to test that everything is working:

 1. Unpack the image using 'docker load':

       docker load < paper371.tar.gz

    When this succeeds Docker displays the image name:

       Loaded image: pldi2019-asp:latest

 2. Run the Docker image in interactive mode:

       docker run -it --rm pldi2019-asp

    You should see a prompt like this:

       opam@b7c56f938d18:~/ocaml-asp$

    (The container id, here 'b7c56f938d18', will vary from run to run.)

 3. Build and run the tests:

       make test

    The last lines printed to stdout should look like this:

       Ran: 5 tests in: 0.25 seconds.
       OK

 4. Build and run a representative benchmark:

       make QUOTA=10 bench-ppm

    This step will take around 5 minutes.

    Findlib warnings ('Interface trx.cmi occurs in several directories' etc.)
    may be safely ignored.

 5. Try out the library interactively

    (a) start the MetaOCaml toplevel:

        metaocaml

    (b) create an unstaged parser (Section 4) for character tokens by entering
        the following statements one by one at the toplevel:

        (* The Parse functor is parameterized by the tag type (lines 703-705).
           The following line instantiates it with Char_tag to create a module
           of parsers over character streams (with the interface in lines
           709-719) *)

        module P = Unstaged.Parse(Utilities.Unstaged.Char_tag);;

        (* Create a sample parser for the Dyck language of balanced brackets.
           The parser returns the number of bracket pairs. *)

        let dyck = P.(fix @@ fun p ->
                      eps 0
                 <|> (tok (Chr '[') >>> p >>> tok (Chr ']') >>> p
                         $ fun (((_,n),_),m) -> n+m+1));;

        (* Type-check the parser.  The 'type_check' function is a wrapper
           around the typeof function (lines 814-815) that passes in an empty
           context.  It returns a parser that has been checked for conformance
           with the system in Section 3. *)

        let checked_parser =  P.type_check dyck;;

        (* The next line defines a utility function 'chr' that builds a token
           from a character. *)

        let chr c = P.Tok (Utilities.Unstaged.Chr c, c);;

        (* Test the parser with some inputs.  First, try an unmatched sequence
           of brackets, for which parsing should fail. *)

        P.parse checked_parser
          @@ Stream.of_list [chr '['; chr ']'; chr '['];;

        (* Next, try a balanced set of brackets, for which parsing should
           succeed, returning '2'. *)

        P.parse checked_parser
          @@ Stream.of_list [chr '['; chr ']'; chr '['; chr ']'];;

    (c) create a staged parser (Section 5) for character tokens by entering
        the following statements one by one at the toplevel:

        (* As for unstaged parsers, the Parse functor is parameterized by the
           tag type.  The following line instantiates it with Char_element to
           create a module of parsers over character sequences *)

        module S = Staged.Parse(Asp_utilities.Char_element);;

        (* The code to create a parser for the Dyck bracket language is very
           similar to the unstaged version, but has some additional staging
           annotations (quotes .< >. and escape .~): *)

        let dyck = S.(fix @@ fun p ->
                      eps .<0>.
                  <|> (tok (Chr '[') >>> p >>> tok (Chr ']') >>> p
                          $ fun p -> .< let (((_,n),_),m) = .~p in n+m+1>.));;

        (* Type-checking is similar to type-checking for unstaged parsers *)

        let checked_parser = S.type_check dyck;;

        (* In order to generate code we must pass a further parameter that
           specializes the generated code to the type of the input stream.
           The library supports various input types: file handles, arrays,
           strings, etc.  The following line specializes for strings *)

        module C = S.Parser(Streamcode.Stringcode);;

        (* It is now possible to generate code specialized for the grammar
           (dyck) and input type (string).  The following line generates the
           specialized code and displays it at the top level *)

        let parser_code = C.compile checked_parser;;

        (* MetaOCaml's Runcode.run function turns the generated code into a
           function that can be run in the toplevel: *)

        let parser = Runcode.run parser_code;;

        (* Finally, here is a call to test the parser; it returns a pair of
           the result (4, for the number of bracket pairs) and the index (8)
           of the next character in the string after the parsed prefix: *)

        parser "[[]][][]" ~index:0;;

## Step-by-Step Instructions

### Interface differences

The paper mentions (line 703) that its presentation of the library
interface is slightly simplified.  Here are the notable differences:

  * The infix operators $ and >>> in the library correspond to the functions
    map and seq in the paper (e.g. lines 710-712)

  * The 'tok' function in the library corresponds to the 'chr' function in the
    paper (e.g. line 712).  This is because the library provides a more
    general interface than the paper: parsers operate on arbitrary tokens, not
    just characters.

  * The eps function in the library accepts an argument: 'eps v' is a parser
    that consumes no tokens, succeeds and returns 'v'.  In the paper 'eps'
    takes no argument, and such a parser can be written 'map (fun () -> v,
    eps)'
  
### Claims supported by the artifact

§2 and §3 give the theory of typed μ-regular expressions and their types.
§4-§6 describe a parser combinator library built using that theory, its staged
implementation and its performance.  This artifact provides support for the
claims about the implementation presented in §4-§6.

We also provide pointers into the implementation for salient features
mentioned in the paper.

#### Claims about parser combinators (§4):

 1. The in-library type system raises an exception for ill-formed grammars
    (lines 98-99, lines 722-724; §4.3)

    The Dyck parser in the Getting Started section corresponds to the
    following grammar

        p ::= ε | [ p ] p

    which satisfies the requirements of our type system.  However, changing
    the order of the sequenced components as follows

        p ::= ε | p [ p ]

    results in a grammar that does not have a valid type in our system,
    because 'p' and '[ p ]' are not "separable" (lines 428-433); p accepts the
    empty string.  The type_check function will raise an exception for such
    grammars.  Here is a definition of a parser corresponding to this second
    grammar:

        module P = Unstaged.Parse(Utilities.Unstaged.Char_tag);;

        let dyck' = P.(fix @@ fun p ->
                      eps 0
                 <|> (p >>> tok (Chr '[') >>> p >>> tok (Chr ']')
                         $ fun (((n,_),m),_) -> n+m+1));;

    The definition is accepted without error, because it is a valid program
    according to OCaml's type system.  However, calling the type_check
    function as follows

        P.type_check dyck';;

    results in an exception:

        Exception:
        Failure
         "seq { null = true; guarded = false; first = { }; follow = { } } { null = false; guarded = true; first = { [ }; follow = { } }".

    Here the 'null = true' indicates that the first grammar in a sequence
    accepts the empty string, a violation of the type system constraints.

 2. The higher-order interface is converted into a first-order representation
    (lines 727-728; §4.2, §4.4)

    The reader may find the first-order definition in the Grammar
    submodule in /home/opam/ocaml-asp/lib/asp_unstaged.ml, lines 122-166.
    and the conversion in the HOAS submodule of the same file, lines 254-325.
    (There is a similar implementation in the staged module, asp_staged.ml)

    The code can also be browsed on GitHub:

       https://github.com/yallop/ocaml-asp/blob/6010e437/lib/asp_unstaged.ml#L122-L166
       https://github.com/yallop/ocaml-asp/blob/6010e437/lib/asp_unstaged.ml#L254-L325

 3. The type checker uses a fixed point iteration to infer types for fixed points
    (lines 818-820)

    See the 'fix' function on lines 84-92 of asp_unstaged.ml

 4. Parsers operate on an imperative stream without backtracking
    (lines 823-843)

    See the use of the imperative Stream module (from OCaml's standard
    library) in the Parser submodule in asp_unstaged.ml, lines
    170-252.

#### Claims about staged parser combinators (§5):

 5. The interface needs only a change to the type of 'map' to support staging
    (lines 928-929)

    The functor calls that build the unstaged and staged parser
    modules above show the types of the resulting combinators:

       module P = Unstaged.Parse(Utilities.Unstaged.Char_tag);;
       module S = Staged.Parse(Asp_utilities.Char_element);;

    The types of most combinators are the same in both cases; for
    example, the types for the 'bot' and alternation parsers are as
    follows in both modules

       val bot : 'a t
       val ( <|> ) : 'a t -> 'a t -> 'a t

    However the type of map (here called '$' -- see above) is
    different.

       (* Unstaged map *)
       val ( $ ) : 'a t -> ('a -> 'b) -> 'b t

       (* Staged map *)
       val ( $ ) : 'a t -> ('a code -> 'b code) -> 'b t

    Since eps takes a value in the code (but not in the paper), its
    type is different, too:

       (* Unstaged eps *)
       val eps : 'a -> 'a t

       (* Unstaged eps *)
       val eps : 'a code -> 'a t

    (See 'Interface differences' for more about the difference in the
    type of 'eps'.)

 6. The internals — in particular, 'peek' — make use of CPS
    (lines 952-970)

    See the functions peek_mem and peek, in
    /home/opam/ocaml-asp/lib/asp_stream_ir.mli
    and /home/opam/ocaml-asp/lib/asp_stream_ir.ml:
    
      val peek_mem : Set.Make(E.Ord).t -> ([`Yes | `No | `Eof] -> 'b t) -> 'b t
      val peek : 'a E.tag list -> ([`Yes of 'a code | `No | `Eof] -> 'b t) -> 'b t

    The peek_mem function takes a set of tokens and a continuation.
    The peek function takes a list of tags and a continuation.
    The peek_mem function is called with the first set in the
    definition of 'alt' in asp_staged.ml, as described in the paper
    (lines 969-972).

 7. The internals make use of The Trick (bounded static variation)
    (lines 978-1008)

    The variant tags `Yes, `No, `Eof in the types of peek and peek_mem
    above correspond to the use of The Trick described in lines
    978-1008 of the paper.

 8. The staged library uses a first-order intermediate representation
    (§5.2)

    The first-order representation described is the 'comp' type in the
    Ir module in asp_stream_ir.ml (lines 75-91)

 9. Types are used to prune branches, to avoiding examining characters twice
    (lines 115-117, lines 1028-1044)

    The Peek_mem and Peek cases in the cdcomp function in
    asp_stream_ir.ml examine the context to determine whether the next
    tag in the stream is known to be (or known not to be) a tag that
    the parser expects to see next.  For example, the following
    snippet

         (* Check what we know already *)
         if Toks.S.(is_empty (inter ctxt.values utagset))
         then (* definitely not in there *) cdcomp ctxt (k' `No)

    checks the intersection between the possible tags for the next
    token (as determined by branches already encountered) and the tags
    that the parser expects to see next.  If the intersection is empty
    then parsing will certainly fail and the continuation k is called
    with `No.  (In contrast, if it is determined that parsing may
    succeed, but not enough is known about the next token to be sure,
    then a branch is inserted into the generated code.)

 10. The staged library uses a MetaOCaml extension for mutual recursion
    (lines 1052-1064)

    See the calls to Genletrec.genletrec in asp_streamcode.ml and
    Genletrec.genletrec_locus in asp_stream_ir.ml

 11. The staged library generates the low-level code shown in the paper
    (§5.4)

    Code that is equivalent (save for some renaming) to the code shown
    in §5.4 may be generated as follows in the metaocaml toplevel:

       (* Instantiate the Staged parser combinator module *)
       module S = Staged.Parse(Asp_utilities.Char_element);;

       (* Abbreviation for the chr' function *) 
       let chr = Utilities.Staged.Charparsing.chr;;

       (* Define the sexp parser *)
       let sexp_parser = 
          S.(fix @@ fun self ->
                   ((chr 'a' $ fun _ -> .<()>.))
                <|> ((chr '(' >>> star self >>> chr ')') $ fun _ -> .<()>.));;

       (* Type-check the sexp parser *)
       let sexp_parser' = S.type_check sexp_parser;;

       (* Compile the sexp parser and display the generated code *)
       module C = S.Parser(Streamcode.Stringcode);;
       C.compile sexp_parser';;

#### Claims about benchmarks (§6):

 12. There are five benchmarks (arith, pgn, ppm, sexp, json)
    (lines 1174-1191)

     (The benchmark 'arith' in the paper is called 'intexp' in the code,
      but the other names match up.)

     A benchmark $B can be run by making the bench-$B target, like this:

        make QUOTA=10 bench-ppm

     The phrase 'QUOTA=10' says that each portion of the benchmark can be run
     for up to 10 seconds, so if a portion takes 100ms (say) then it will be
     run around 100 times.  If the specified quota is high then the benchmark
     will take a long time to run; however, if it is low then there may be
     too few runs to support the statistics, and you will see a message like this:

         Error (unstaged_ppm:51076
          "Columns  Runs per sampled batch(non-zero 5) have less that 10 non-zero values.")

     In practice only the 'unstaged' portions (which are of lesser interest
     than the 'staged' and 'ocamlyacc' portions) will take a long time, and
     you may be able to reproduce the results to your satisfaction with a
     fairly small QUOTA.

     Sample output of a benchmark is given in /home/opam/intexp-benchmark.txt,
     and looks like this:

         Name                       Time R^2     Time/Run                95ci  
        -------------------------- ---------- ------------ ------------------- 
         ocamlyacc_intexp:262144        1.00      24.85ms     -0.08ms +0.10ms  
         ocamlyacc_intexp:524288        1.00      50.31ms     -0.13ms +0.16ms  
         ...

     The components in the table are as follows:

        * 'Name' gives the framework used for the parser ('ocamlyacc', 'unstaged'
           or 'staged'), the benchmark (here 'intexp'), and the size of the input
           (e.g. 262144)

        * 'Time R^2' gives the coefficient of determination
          (https://en.wikipedia.org/wiki/Coefficient_of_determination)

        * 'Time/Run' gives the time taken to parse the input

        * 95ci is the 95% confidence interval
          (https://en.wikipedia.org/wiki/Confidence_interval)

     The code and input data for the benchmarks may be found in the
     subdirectories of /home/opam/ocaml-asp/benchmarks.  For example, the
     directory /home/opam/ocaml-asp/benchmarks contains the following files:
     
     * README.md
       brief description of the benchmark
     * data
       directory containing benchmark input data
     * intexp_benchmark.ml
       main entry point for the benchmark
     * intexp_lexer.mll
       ocamllex lexer for intexp tokens
     * intexp_parser.mly
       ocamlyacc parser for the intexp grammar
     * intexp_unstaged_combinator_parser.ml
       unstaged lexer & parser written using our library
     * intexp_staged_combinator_parser.ml
       staged lexer & parser written using our library
     * intexp_tokens.ml
       definitions of token functions used by our library
     * intexp_tokens.mli
       interface to token functions
     * intexp_tokens_base.ml
       definition of token types

 13. Staging improves parser throughput to a point comparable to ocamlyacc
    (lines 120-121, Figure 7)

    The throughput figures may be computed from the output of the benchmarks
    as described above.  For example, the sample output table for the intexp
    benchmark contains the following entries:

      staged_intexp:2097152          1.00     144.10ms     -0.36ms +0.46ms  
      ocamlyacc_intexp:2097152       1.00     204.51ms     -0.48ms +0.45ms  

    The first of these states that the staged parser combinators take 144.10ms
    to parse 2097152 bytes (2MB) of data, a throughput of 13.87 MB/s
    (i.e. 2/0.1441).

    The second states that ocamlyacc take 204.51ms to parse the same data, a
    throughput of 9.77 MB/s.

    These numbers are fairly comparable to (actually, slightly more favourable
    than, but exact results will vary) the numbers given in Figure 7.

 14. Running time is linear in the length of input
    (lines 102-103, lines 1227-1231, Figure 8)

    Again, these figures may be computed from the benchmark outputs.  For
    example, here are the figures for the staged benchmark from
    intexp-output.txt:
    
      staged_intexp:262144           1.00      17.33ms     -0.09ms +0.09ms  
      staged_intexp:524288           1.00      34.82ms     -0.19ms +0.22ms  
      staged_intexp:786432           1.00      52.01ms     -0.07ms +0.08ms  
      staged_intexp:1048576          1.00      69.98ms     -0.27ms +0.30ms  
      staged_intexp:1310720          1.00      88.07ms     -0.10ms +0.10ms  
      staged_intexp:1572864          1.00     107.33ms     -0.24ms +0.24ms  
      staged_intexp:1835008          1.00     127.10ms     -0.45ms +0.41ms  
      staged_intexp:2097152          1.00     144.10ms     -0.36ms +0.46ms  

    A quick glance shows a linear relationship: the time taken to parse 0.5MB
    (524288 bytes), 34.82ms is approximately half the time taken to parse 1MB
    (1048576 bytes), 69.98ms, and approximately a quarter of the time taken to
    parse 2MB (2097152 bytes), 144.10ms.
