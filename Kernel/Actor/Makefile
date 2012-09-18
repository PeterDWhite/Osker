# $Id
# Copyright (C) Peter Duncan White, 2001, 2002, 2003
# Copyright (c) OHSU, 2001, 2002, 2003
#  Makefile for the actor portion of Osker

#  Get directories involves in the make
include ../../dirs.mk

HC        = ghc
INCS      = $(POSIX):$(UTILITIES):$(GRAPH):$(DOM):$(RESUMPTION):$(BR):$(GLOBAL):$(KPROC)
HCFLAGS   = -cpp -O2 $(DEFS) -i$(INCS) $(OPTS) -Wall -syslib data
HS_IGNORE = Structure.hs
HS_SRCS   = ${filter-out ${HS_IGNORE}, $(wildcard *.hs)}
OBJS      = $(addsuffix .o,$(basename $(HS_SRCS)))
MAIN      = target

$(MAIN):
# trick  :-)
ifeq (.depend,$(wildcard .depend))
all: $(MAIN)
include .depend
else
all: depend $(MAIN)
endif

%.o: %.hs
	$(HC) $(HCFLAGS) -c $< -o $@

%.hi: %.o
	@\:

$(MAIN): depend
	make depend
	make quick

quick:  $(OBJS)
	touch $(MAIN)

clean:
	$(RM) $(OBJS) *.hi core $(MAIN) profile.* *~ .depend*
	$(RM) *.hcr *.hp

depend: $(HS_SRCS)
	$(RM) .depend
	$(HC) -M -optdep-f -optdep.depend $(HCFLAGS) $(HS_SRCS)
