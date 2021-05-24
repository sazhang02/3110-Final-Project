MODULES=gui levels board player_state boss_state main final_level homescreen
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=gui.mli levels.mli board.mli player_state.mli boss_state.mli final_level.mli homescreen.mli
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=unix,ounit2,graphics,yojson,camlimages.graphics,camlimages.all_formats

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
	zip stuck_in_the_desert.zip *.ml* *.json _tags images/*.png images/30/*.png images/40/*.png images/50/*.png .merlin .ocamlformat .ocamlinit LICENSE Makefile *.md

clean:
	ocamlbuild -clean
		rm -rf stuck_in_the_desert.zip test.byte main.byte _doc.private _doc.public

docs: docs-public docs-private

docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)