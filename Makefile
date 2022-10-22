aritha: x86_64.cmo analyseur_lexical.cmo analyseur_syntaxique.cmo assembly.cmo aritha.ml
	ocamlc -o aritha x86_64.cmo analyseur_lexical.cmo analyseur_syntaxique.cmo assembly.cmo aritha.ml

assembly.cmo: x86_64.cmo analyseur_syntaxique.cmo assembly.cmi assembly.ml
	ocamlc -c x86_64.cmo analyseur_syntaxique.cmo assembly.ml
assembly.cmi: analyseur_syntaxique.cmi assembly.mli
	ocamlc -c assembly.mli

analyseur_syntaxique.cmo: analyseur_lexical.cmo analyseur_syntaxique.cmi analyseur_syntaxique.ml
	ocamlc -c analyseur_lexical.cmo analyseur_syntaxique.ml
analyseur_syntaxique.cmi: analyseur_lexical.cmi analyseur_syntaxique.mli
	ocamlc -c analyseur_syntaxique.mli

analyseur_lexical.cmo: analyseur_lexical.cmi analyseur_lexical.ml
	ocamlc -c analyseur_lexical.ml
analyseur_lexical.cmi: analyseur_lexical.mli
	ocamlc -c analyseur_lexical.mli

x86_64.cmo: x86_64.cmi x86_64.ml
	ocamlc -c x86_64.ml
x86_64.cmi: x86_64.mli
	ocamlc -c x86_64.mli

clean:
	rm *.cmi *.cmo aritha
