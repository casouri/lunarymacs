.RECIPEPREFIX = >

all: help

mac-install: en_US.dic en_US.aff
>cp en_US.* ~/Library/Spelling/ ; \
cd ~/Library/Spelling ; \
ln -s ./en_US.aff ./default.aff ; \
ln -s ./en_US.dic ./default.dic

mac-remove:
>trash ~/Library/Spelling/en_US.* ~/Library/Spelling/default.*

help:
>echo "Avaliable commands: mac-install mac-remove"
