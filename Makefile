.RECIPEPREFIX = >

EMACS_FLAGS=--eval '(load "~/.emacs.d/init.el")'
EMACS=emacs --quick --batch $(EMACS_FLAGS)

.PHONY: install autoload autoremove update clean export-doc help

all: install autoload autoremove

help:
>@echo "Avaliable commands:\ninstall  autoload  autoremove  update  clean"

install: init.el .local/autoloads.el
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
$(EMACS) -f moon/update-package

clean:
>@echo "Removing compiled files" ;\
rm -f *.elc

export-doc:
>@$(EMACS) --eval "(progn (moon-initialize-load-path) (moon-load-autoload) (moon/export-doc-to-wiki))" ;\
cd wiki ;\
git pull --rebase ;\
git stage . ;\
git commit -m "update" ;\
git push

install-emacs25:
>brew install emacs-plus --without-spacemacs-icon --with-natural-title-bar

install-emacs26:
>brew install emacs-plus --without-spacemacs-icon --devel
