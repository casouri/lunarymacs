.RECIPEPREFIX = >

EMACS_FLAGS=--eval '(load "~/.emacs.d/init.el")'
EMACS=emacs --quick --batch $(EMACS_FLAGS)

all: install autoload

install: init.el .local/autoloads.el
>@$(EMACS) -f moon/install-package

autoload: init.el
>@$(EMACS) -f moon/generate-autoload-file

clean:
>rm -f .*/**/*.elc;rm -f */**/*.elc
