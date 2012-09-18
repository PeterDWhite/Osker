# $Id
# Copyright (C) Peter Duncan White, 2001, 2002, 2003
# Copyright (c) OHSU, 2001, 2002, 2003
#  Makefile for the device driver portion of Osker

#  Get directories involves in the make
include ../../dirs.mk

HC        = ghc
INCS      = $(POSIX):$(UT):$(GRAPH):$(DOM):$(ALLOC):$(ACTOR):$(RES):$(BR):$(SYS2):$(EXEC):$(MSG)
HCFLAGS   = -cpp $(DEFS) -i$(INCS) $(OPTS) -Wall -syslib concurrent -syslib data -syslib posix
HS_IGNORE = 
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
