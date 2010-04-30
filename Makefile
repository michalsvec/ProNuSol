#
# Project: jabberclient
# Author:  XXXX
#
# Usage:
#   - compile project:       make
#   - clean:                 make clean
#   - pack (zip):            make pack
#

# Compiler
PL = pl
#HCFLAGS = -Wall -O2 -threaded --make -fglasgow-exts
HCFLAGS = -Wall -O2 -threaded -package qt --make -i./lib/XMPP-0.0.1 -odir $(BUILDDIR) -hidir $(BUILDDIR)

# Output paths
OUTDIR = .
BUILDDIR = $(OUTDIR)/build
OUTPROJFILE = $(OUTDIR)/jabclient
#OUTPROJFILE = $(OUTDIR)/test

# Source paths
SRCDIR = ./src
#SRCFILES = ./test.hs
SRCFILES = $(SRCDIR)/gui/Main.hs \
	$(SRCDIR)/gui/Global.hs \
	$(SRCDIR)/xmpp/*.hs

# Project compilation
all: $(SRCFILES)
	$(HC) $(HCFLAGS) -o $(OUTPROJFILE) -c $(SRCFILES)

# Cleaning
clean:
	rm -rf $(BUILDDIR)/*.o $(BUILDDIR)/*.hi
	rm -f $(OUTPROJFILE)

# Packing
pack:
	make clean
	zip -r xxx.zip $(SRCDIR) Makefile README

# End of file
