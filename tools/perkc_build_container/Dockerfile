FROM ocaml/opam:debian-11-ocaml-5.3

# Install dependencies
RUN sudo apt-get update && sudo apt-get install -y universal-ctags
RUN opam update && opam install -y \
    ppx_deriving \
    sedlex \
    menhir \
    odoc \
    cmdliner \
    fpath