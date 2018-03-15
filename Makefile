NAME = flatzinc

#MENHIR = menhir -v --ocamlc 'ocamlbuild -use-ocamlfind'
MENHIR = menhir -v --infer
BUILD = ocamlbuild -menhir "$(MENHIR)" -use-ocamlfind -j 0 #-quiet

all: $(NAME).cma $(NAME).cmxa
	@echo "Building successful!"

%:
	@echo "Building $@..."; $(BUILD) $@

doc:
	@$(BUILD) -quiet $(NAME).docdir/index.html

clean:
	@$(BUILD) -clean -quiet; echo "Cleaning successful"

update : uninstall install
install :
	@ocamlfind install $(NAME) META *.mli _build/*.cmi _build/$(NAME).cma _build/$(NAME).cmxa _build/$(NAME).a
uninstall :
	@ocamlfind remove $(NAME)

.PHONY: all doc clean install uninstall update
