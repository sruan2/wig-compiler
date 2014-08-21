all: 
	ocamlbuild -use-menhir main.native

check:  all
	./run_check

clean:
	rm -r _build main.native; rm result
