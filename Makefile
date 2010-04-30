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
PCFLAGS = -q

# Output paths
OUTDIR = .
OUTPROJFILE = $(OUTDIR)/nurikabe

# Source paths
SRCDIR = .
SRCFILES = $(SRCDIR)/nurikabe.pl

# Project compilation
all: $(SRCFILES)
	$(PL) $(PCFLAGS) -o $(OUTPROJFILE) -c $(SRCFILES)

# Cleaning
clean:
	rm -f $(OUTPROJFILE)

# Packing
pack:
	make clean
	zip -r fpr-log-xsrbpa00.zip $(SRCFILES) README Makefile rozdeleni ./test/*

# End of file
