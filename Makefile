#
#  Program   : nurikabe.hs
#  Copyright : tvrdaci 2010
#  Authors   : Jiri Melichar, Pavel Srb, Michal Svec
#  Project   : FPR
#  Version   : 0.0
#  Modified  : 30.04.2010
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
	zip -r fpr-log-xsrbpa00.zip $(SRCFILES) README Makefile rozdeleni runpl.sh ./test/*

# End of file
