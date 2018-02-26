.RECIPEPREFIX = >

EMACS_FLAGS=--eval '(load "~/.emacs.d/init.el")'
EMACS=emacs --quick --batch $(EMACS_FLAGS)

.PHONY: install autoload autoremove update clean doc help update-moon

# install have to be in the front
# otherwise use-package will not be find
# on fresh install
all: | install autoload autoremove

help:
>@echo "Avaliable commands:\ninstall  autoload  autoremove  update  clean update-moon"

.local:
>mkdir .local .local/package .local/package/elpa

custom.el: .local
>touch .local/custom.el

autoloads.el: .local
>touch .local/autoloads.el

# commands
install: init.el autoloads.el .local custom.el
>@echo "Installing packages" ;\
$(EMACS) -f moon/install-package

autoload: init.el
>@echo "Generating autoload files" ;\
$(EMACS) -f moon/generate-autoload-file

autoremove: init.el .local/autoloads.el
>@echo "Removing unused packages" ;\
$(EMACS) -f moon/remove-unused-package ;\
rm -f .local/autoloads.el~

update: init.el .local/autoloads.el
>@echo "Updating packages" ;\
$(EMACS) -f moon/update-package -f moon/generate-autoload-file

update-moon:
>git pull --rebase

clean:
>@echo "Removing compiled files" ;\
rm -f *.elc

install-emacs25:
>brew install emacs-plus --without-spacemacs-icon --with-natural-title-bar

install-emacs26:
>brew install emacs-plus --without-spacemacs-icon --devel
