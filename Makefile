OCAMLBUILD=ocamlbuild -use-ocamlfind -ocamlc '-toolchain metaocaml ocamlc' \
                                     -ocamlopt '-toolchain metaocaml ocamlopt' \
                                     -ocamldep 'ocamldep -as-map'

BENCHMARKS=json intexp pgn ppm sexp

all: lib
lib:
	$(OCAMLBUILD) asp.cma asp.cmxa

bench: lib \
       $(foreach bench,$(BENCHMARKS),${bench}_benchmark.native) \
       $(foreach bench,$(BENCHMARKS),bench-${bench})

bench-%: %_benchmark.native
        # Some generated code is currently not tail-recursive,
        # so we need a bigger stack.
	ulimit -s unlimited
	cp _build/lib/*.cm* _build/benchmarks/$*
	cd _build/benchmarks/$*   && \
           ./$*_benchmark.native  -quota 1                \
                                  -ci-absolute            \
                                  -ascii -display blank   \
                                  -clear-columns          \
                                  -all-values             \
                                  +time

%.native %.cma %.cmxa:
	$(OCAMLBUILD) $@

test:
	$(OCAMLBUILD) test.native
	chmod +x test.native
	cp _build/lib/*.cm* _build/lib_test/$*
	cp lib_test/*.ok lib_test/*.bad _build/lib_test/$*
	cd _build/lib_test && ./test.native

install: lib
	ocamlfind install asp META		\
	   _build/lib/asp.cmi			\
	   _build/lib/asp_streamcode.cmi	\
	   _build/lib/asp_types.cmi		\
	   _build/lib/asp_staged.cmi		\
	   _build/lib/asp_unstaged.cmi		\
	   _build/lib/asp_utilities.cmi		\
	   _build/lib/asp_utilities_staged.cmi	\
           _build/lib/asp.cma			\
           _build/lib/asp.cmxa			\
           _build/lib/asp.a


uninstall:
	ocamlfind remove asp

clean:
	$(OCAMLBUILD) -clean

.PHONY: bench test all lib install uninstall
