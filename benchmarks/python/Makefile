
all: stuff.cma

stuff.cma: pytok.ml tokenizer.o tokenizer_stubs.o
	ocamlmklib tokenizer.o tokenizer_stubs.o pytok.ml -linkall -o stuff

tokenizer.o: tokenizer.c tokenizer.h token.h errcode.h
	ocamlc -c -g tokenizer.c

tokenizer_stubs.o: tokenizer_stubs.c tokenizer.h token.h errcode.h
	ocamlc -c -g  tokenizer_stubs.c

clean:
	rm -f *.cmi *.cmo *.cmx *.cma *.cmxa *.o *.so *.a
