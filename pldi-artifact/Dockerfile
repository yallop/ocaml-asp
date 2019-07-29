FROM ocaml/opam2:debian-stable

RUN sudo apt-get install --yes m4
RUN opam switch create 4.07.1+BER
RUN opam install --yes ocamlfind ocamlbuild

RUN opam config exec -- opam pin add letrec https://github.com/yallop/metaocaml-letrec.git#d182d411
RUN opam install --yes core_bench.v0.11.0 ounit

# Install the ocaml-asp library and set up the environment
RUN git clone https://github.com/yallop/ocaml-asp /home/opam/ocaml-asp
WORKDIR /home/opam/ocaml-asp
RUN git checkout ba3ee6b8
RUN opam config exec -- make

RUN opam pin add --yes `pwd`
RUN opam config exec -- make all test
ADD files/.ocamlinit /home/opam/ocaml-asp/.ocamlinit
ADD files/intexp-output.txt /home/opam/intexp-benchmark.txt
