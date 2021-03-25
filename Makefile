MODULES=gui levels player_state
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
# OCAMLBUILD=ocamlbuild -use-ocamlfind -pkg camlimages.graphics -pkg camlimages.png
#  OCAMLBUILD=ocamlbuild -use-ocamlfind -pkg camlimages
default: build
	eval $(opam config env)
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential