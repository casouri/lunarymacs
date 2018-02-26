.RECIPEPREFIX = >

EMACS_FLAGS=--eval '(load "~/.emacs.d/init.el")'
EMACS=emacs --quick --batch $(EMACS_FLAGS)

.PHONY: install autoload autoremove update clean doc help

all: install autoload autoremove

help:
>@echo "Avaliable commands:\ninstall  autoload  autoremove  update  clean"

.local:
>mkdir .local \
.local/package \
.local/package/elpa

custom.el: .local
>touch .local/custom.el

autoloads.el: .local
>touch .local/autoloads.el

install: init.el autoloads.el .local
>@echo "Installing packages" ;\
$(EMACS) -f moon/install-package

autoload: init.el
>@echo "Generating autoload files" ;\
$(EMACS) -f moon/generate-autoload-file

autoremove: init.el .local/autoloads.el
>@echo "Removing unused packages" ;\
$(EMACS) -f moon/remove-unused-package

update: init.el .local/autoloads.el
>@echo "Updating packages" ;\
$(EMACS) -f moon/update-package -f moon/generate-autoload-file

clean:
>@echo "Removing compiled files" ;\
rm -f *.elc

install-emacs25:
>brew install emacs-plus --without-spacemacs-icon --with-natural-title-bar

install-emacs26:
>brew install emacs-plus --without-spacemacs-icon --devel
