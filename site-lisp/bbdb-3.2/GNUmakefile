# Rules to generate the files that need to go into the ELPA package.

# Copied and adapted from AUCTeX's GNUmakefile.

MAKEINFO=makeinfo
INSTALL_INFO=install-info

MANUALS=bbdb
INFO_FILES=$(MANUALS:=.info)

# FIXME: Currently these files need to be stored in the elpa.git repository
# because the elpa.gnu.org scripts don't know how to build the Info file
# from the Texinfo file.
GENERATED_FILES=dir $(INFO_FILES)

elpa: $(GENERATED_FILES)

clean:
	rm -f $(GENERATED_FILES)

TEXI_SOURCES:=$(wildcard doc/*.texi)
$(INFO_FILES): %.info: $(TEXI_SOURCES)
	cd doc; $(MAKEINFO) --no-split $*.texi
	mv doc/$*.info $@

dir: $(INFO_FILES)
	for f in $(INFO_FILES); do $(INSTALL_INFO) --info-dir=. $$f; done
