MODULES=gui levels board player_state main
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	eval $(opam config env)
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential
	
play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

zip:
	zip desert.zip *.ml* *.json _tags images/*.png .merlin .ocamlformat .ocamlinit LICENSE Makefile *.md