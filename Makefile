MODULES=gui
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
OCAMLBUILD=ocamlbuild -use-ocamlfind
# OCAMLBUILD=ocamlbuild -use-ocamlfind -pkg camlimages.graphics -pkg camlimages.png
#  OCAMLBUILD=ocamlbuild -use-ocamlfind -pkg camlimages
default: build
	eval $(opam config env)
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)