all: help

mac-install: en_US.dic en_US.aff
	cp en_US.* ~/Library/Spelling/ ; \
	cd ~/Library/Spelling ; \
	ln -s ./en_US.aff ./default.aff ; \
	ln -s ./en_US.dic ./default.dic

mac-remove:
	trash ~/Library/Spelling/en_US.* ~/Library/Spelling/default.*

/usr/share/hunspell:
	sudo mkdir /usr/share/hunspell

linux-install: en_US.dic en_US.aff /usr/share/hunspell
	sudo cp en_US.* /usr/share/hunspell

linux-remove:
	sudo rm /usr/share/hunspell/en_US.*

help:
	echo "Avaliable commands: mac-install mac-remove linux-install linux-remove"
