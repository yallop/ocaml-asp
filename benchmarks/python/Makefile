MLS=$(wildcard *.ml)

all: stuff.cma

stuff.cma: $(MLS) $(wildcard *.mli) tokenizer.o tokenizer_stubs.o
	ocamlfind -toolchain metaocaml c -package asp python_tokens_base.ml $(wildcard *.mli)
	ocamlfind -toolchain metaocaml ocamlmklib -package asp tokenizer.o tokenizer_stubs.o $(MLS) -linkall -o stuff

tokenizer.o: tokenizer.c tokenizer.h token.h errcode.h
	ocamlfind -toolchain metaocaml ocamlc -c -g tokenizer.c

tokenizer_stubs.o: tokenizer_stubs.c tokenizer.h token.h errcode.h
	ocamlfind -toolchain metaocaml ocamlc -c -g  tokenizer_stubs.c

clean:
	rm -f *.cmi *.cmo *.cmx *.cma *.cmxa *.o *.so *.a
