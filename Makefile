build:
	ocamlbuild -use-ocamlfind xxMain.native

clean:
	ocamlbuild -use-ocamlfind -clean

default: build
