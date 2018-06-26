.RECIPEPREFIX = >

EMACS_FLAGS=-l init.el
EMACS=emacs --quick --batch $(EMACS_FLAGS)

.PHONY: install autoload clean doc help update-moon

# install have to be in the front
# otherwise use-package will not be avaliable
# on fresh install
# all: | install autoload autoremove
all: custom.el autoloads.el
>@$(EMACS) --eval '(progn (message "Checking packages...") (moon/use-package) (message "Generating autoload file...") (moon/generate-autoload-file))' ;\
rm -f .local/autoloads.el~


help:
>@echo "Avaliable commands:\ninstall  autoload  clean update-moon"

.local:
>mkdir .local .local/package/

custom.el: .local
>touch .local/custom.el

autoloads.el: .local
>touch .local/autoloads.el

# commands
install: init.el autoloads.el .local custom.el
>@echo "Installing packages" ;\
$(EMACS) -f moon/use-package

autoload: init.el
>@echo "Generating autoload files" ;\
$(EMACS) -f moon/generate-autoload-file ;\
rm -f .local/autoloads.el~

update-moon:
>git pull --rebase

clean:
>@echo "Removing compiled files" ;\
rm -f *.elc

test:
>emacs --eval "(add-hook 'moon-post-init-hook #'moon/run-test t)"
