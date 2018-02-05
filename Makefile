.RECIPEPREFIX = >

EMACS_FLAGS=--eval '(load "~/.emacs.d/init.el")'
EMACS=emacs --quick --batch $(EMACS_FLAGS)

install: init.el .local/autoloads.el
>@$(EMACS) -f moon/install-package

autoload: init.el
>@$(EMACS) -f moon/generate-autoload-file
