## -*- Mode: makefile -*-
## 
## $Source: /psyc/lang/soar/emacs/utils/dismal/new/Makefile,v $
## 
## Description       : Makefile for dismal
## Original author(s): Frank Ritter	Frank.Ritter@nottingham.ac.uk
## Organization      : Dept. of Psychology, University of Nottingham
## 
## To use this makefile, first check the name of your emacs.  Then:
##
## 1) If the name of your emacs executable is the same as the
##    value of the definition of "EMACS" below, then you can make
##    this system by simply cd-ing to the directory of this file and 
##    typing "make" in a shell.
##
## 2) If the name of your emacs executable is different, then
##    can make this system by typing
##               make EMACS=xxxx
##    where "xxxx" is the name of your emacs executable.
##    or by changing the alias for EMACS defined below.
##
## To release (in case you don't remember):
## (a) change RELEASE line below and save this file
## (b) change dismal-version in dismal.el and save the file
## (d) change version in LCD-entry
## (e) change verison in README
## (c) cd ~/tools/emacs/dismal/new/
## (f) type "make" to compile dismal (on keats or maybe vpsyc
##      (upsyc is not set up right)
## (f2) goto granby or pine to makeinfo info nodes, or M-x makeinfo-buffer
## (g) type "make package" to make a new directory, tar file, etc.
## (h) change dismal-version in dismal.el back to +
## (i) put copy out on ftp.nottingham
##     ftp ftp.nottingham
##     cd pub
##     binary
##     put dismal-1.3a/dismal-1.3a.tar.gz  dismal-1.3a.tar.gz
##     quit
## (j) put a copy on vpsyc
##     cp ~/tools/emacs/dismal/dismal-1.3/dismal-1.3.tar.gz ~/ftp-ritter/dismal-1.3.tar.gz
## Optional arguments to make:
##    all - same as no argument, compiles
##    package - compiles and makes version for distribution
##    clean   - remove all .elc files and any other cruft
## (j) change dismal-version in dismal.el and save the file

# First define all of our aliases.
# Beware, if "emacs" is aliased to "emacs -q", then -batch below will be 
# treated as a file.
EMACS    = emacs
COMPILER = cc

# no user changeable variables below here.
RELEASE = dismal-1.4

# main files that will be loaded during compiles
# in .elc format.
OBJS    = dismal.elc \
	  dismal-simple-menus.elc \
	  dismal-metacolumn.elc \
	  dismal-mouse3.elc \
	  dismal-menu3.elc \
	  dismal-mode-defaults.elc \
	  auto-aligner.elc \
	  dismal-model-extensions.elc \
	  semi-coder.elc

# supporting files that will be used as utilities
# in .elc format.
EXTRAOBJS = float-changes.elc \
          vectors.elc \
          heaps.elc \
          rmatrix.elc \
          dismal-data-structures.elc \
          soar-misc.elc \
          simple-menu.elc \
	  keystroke.elc \
	  log.elc \
	  dismal-mode-defaults.elc

# files that will be compiled
SRCS    = dismal-data-structures.el \
	  dismal.el \
	  dismal-simple-menus.el \
          dismal-metacolumn.el \
	  dismal-mouse3.el \
	  dismal-menu3.el \
          float-changes.el \
          vectors.el \
          heaps.el \
          rmatrix.el \
          dismal-mode-defaults.el \
          soar-misc.el \
          simple-menu.el \
	  auto-aligner.el \
	  dismal-model-extensions.el \
	  semi-coder.el \
	  keystroke.el \
	  make-km-aliases.el \
	  log.el \
	  emergency.el

# files along for the ride not compiled
MISC 	= COPYING \
	  Makefile \
	  README \
	  REFCARD \
	  LCD-entry \
	  example-codes.txt \
	  aligner-test-data.txt \
	  examples/keystroke4.dis \
	  examples/soar704-aliases.dis \
	  examples/simple-keystroke.dis \
	  examples/test.dis \
	  checkout-dismal.script \
	  dismal-manual.tex \
	  dismal.info \
	  dismal.info-1 \
	  dismal.info-2 \
	  timer.c
#	  dismal-manual.ps
#	  dismal-manual.rtf

all:	${EXTRAOBJS} ${OBJS} tags log

# "make package" will make this package.
# get copies of the util files moved up
# then clean up the old tar file and make new one
package: ${SRCS} ${MISC}
	rm -fr ../${RELEASE}/*
	rm -fr ../${RELEASE}
	mkdir ../${RELEASE}
#	Copy latest utilities up
#	cp ./utilities/goto-manual.el .
	cp ./utilities/x-mouse.el .
	cp ${SRCS} ../${RELEASE}
	cp ${MISC} ../${RELEASE}
#	cp ${OBJS} ../${RELEASE}
#	rm ./goto-manual.el
#	rm ./simple-menu.el
#	rm ./soar-misc.el
#	rm ./x-mouse.el
	cd ..; tar clf ${RELEASE}.tar ${RELEASE}/*
	cp ../${RELEASE}.tar ../${RELEASE}
	cd ..; gzip ${RELEASE}.tar
	mv ../${RELEASE}.tar.gz ../${RELEASE}
	chmod og+xr ../${RELEASE}
	chmod og+r ../${RELEASE}/*

clean:
	rm -f ${OBJS} ${EXTRAOBJS} TAGS

tags:
	etags *.el

log:
	${COMPILER} timer.c -o timer.bin

.SUFFIXES: .elc .el .tar .Z

${EXTRAOBJS}:
	${EMACS} -batch -q -f batch-byte-compile $(@:.elc=.el)


# dismal-mode-defaults is most important, for it makes sure that current 
# directory is on load-path
BASICLOADS =  -l ./dismal-mode-defaults.elc dismal-data-structures.elc \
	./float-changes.elc ./vectors.elc ./heaps.elc ./rmatrix.elc \
	./soar-misc.elc \
	./simple-menu.elc
#	./goto-manual.elc

.el.elc:
	${EMACS} -batch -q ${BASICLOADS} -f batch-byte-compile $(@:.elc=.el)

# Special rules.

#dismal.elc:  
#	${EMACS} -batch -q -l  \
#		./float-changes.elc -f batch-byte-compile dismal.el

# Dependencies.


## Not used yet: (taken from edb makefile)
## info: database.texi
## done by hand
## 	makeinfo -o dismal.info dismal-manual.tex
## 	texi2dvi dismal-manual.tex
##  (or texinfo-format-buffer and save....)
