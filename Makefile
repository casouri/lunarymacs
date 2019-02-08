# .RECIPEPREFIX = >

# EMACS=emacs --quick --eval "(setq moon-setup t)" --eval "(toggle-debug-on-error)" -l init.el --eval "(moon-load-config moon-star-path-list)"
EMACS=emacs --quick --batch --eval "(setq moon-setup t)" -l init.el --eval "(moon-load-config moon-star-path-list)"

.PHONY: install autoload autoremove update clean doc help update-moon

# install have to be in the front
# otherwise use-package will not be available
# on fresh install
# all: | install autoload autoremove
all: custom.el autoload.el
	@$(EMACS) -f moon/make

help:
	@echo "Avaliable commands:\ninstall  autoload  autoremove  update  clean update-moon"

.local:
	mkdir .local .local/package .local/package/elpa

custom.el: .local
	touch .local/custom.el

autoload.el: .local
	touch .local/autoload.el

# commands
install: init.el autoload.el .local custom.el
	@echo "Installing packages" ;\
$(EMACS) -f moon/install-package

autoload: init.el
	@echo "Generating autoload files" ;\
$(EMACS) -f moon/generate-autoload-file ;\
rm -f .local/autoload.el~

autoremove: init.el
	@echo "Removing unused packages" ;\
$(EMACS) -f moon/remove-unused-package

update: init.el
	@echo "Updating packages" ;\
$(EMACS) -f moon/update-package
update-moon:
	git pull --rebase

clean:
	@echo "Removing compiled files" ;\
find . -type f -name *.elc -delete

test:
	emacs --eval "(add-hook 'moon-startup-hook-2 #'moon/run-test nil t)"
