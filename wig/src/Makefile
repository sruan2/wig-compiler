all: 	code check

code:	
	ocamlbuild -use-menhir main.native
	mv main.native wigA

check:  syntax weed pretty symbol type prettytype codegen

syntax:	code
	./scripts/syntax.sh	/* Test Syntax */

weed:	code
	./scripts/weed.sh	/* Test Weeder */

pretty: code
	./scripts/pretty.sh	/* Tests Pretty Printer */

symbol: code
	./scripts/symbol.sh	/* Tests Symbol Tables */

type: code
	./scripts/type.sh	/* Tests Typecheck Tables */

prettytype: code
	./scripts/prettytype.sh	/* Tests Pretty Printing With Type Annotations */

codegen: code
	./scripts/codegen.sh	/* Tests Code Generation */

wig10:
	./scripts/wig10.sh	/* Test Wig10 */

pwig:
	./scripts/pwig.sh	/* Test pwig */

install:
	./scripts/install.sh 	/* Install CGI files manually */

html:
	./scripts/html.sh 	/* Generates an html page for CGI scripts */

clean:
	rm -r _build wigA *.c *.install;
