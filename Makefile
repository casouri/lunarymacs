.RECIPEPREFIX = >

EMACS_FLAGS=--eval '(load "~/.emacs.d/init.el")'
EMACS=emacs --quick --batch $(EMACS_FLAGS)

.PHONY: install autoload autoremove update clean export-doc

all: install autoload

install: init.el .local/autoloads.el
>@$(EMACS) -f moon/install-package

autoload: init.el
>@$(EMACS) -f moon/generate-autoload-file

autoremove: init.el .local/autoloads.el
>@$(EMACS) -f moon/remove-unused-package

update: init.el .local/autoloads.el
>@$(EMACS) -f moon/update-package

clean:
>rm -f .*/**/*.elc;rm -f */**/*.elc

export-doc:
>@$(EMACS) -f moon/export-doc-to-wiki; cd wiki; git stage .;git commit -m "update";git push
