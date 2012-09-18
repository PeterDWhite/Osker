# $Id
# Copyright (C) Peter Duncan White, 2001, 2002, 2003
# Copyright (c) OHSU, 2001, 2002, 2003
#  Makefile for kernel file system directory
#  This directory contains the file system, and its device driver

include ../../dirs.mk

HC        = ghc
INCS      = $(GRAPH):$(UT):$(RES):$(BR):$(POSIX):$(DOM):$(ALLOC):$(ACTOR):$(MSG):$(DAVINCI):$(SYS2):$(EXEC)
HCFLAGS   = -cpp -O2 $(DEFS) -i$(INCS) -Wall $(OPTS) -syslib text -syslib data
HS_IGNORE = Test.hs
HS_SRCS   = ${filter-out ${HS_IGNORE}, $(wildcard *.hs)}
OBJS      = $(addsuffix .o,$(basename $(HS_SRCS)))
TESTS     = $(basename $(HS_IGNORE))
MAIN      = target
DOTOS     = $(OBJS) $(OGRAPH) $(OPOSIX) $(OHIO) $(ORESUMPTION) $(OUTILITIES) $(ODOMAIN) $(ODAVINCI) $(OBIO) $(OMSG) $(OACTOR) $(OALLOCATION)

$(MAIN):
# trick  :-)
ifeq (.depend,$(wildcard .depend))
all: $(MAIN)
include .depend
else
all: depend $(MAIN)
endif

%.o: %.hs
	@echo "Making $@"
	$(HC) $(HCFLAGS) -c $< -o $@

%.hi: %.o
	@\:

%: %.hs
	ghc -o $@ $(HCFLAGS) $< $(DOTOS)
	rm -f $@.o
	rm -f $@.hi

$(MAIN): depend
	make depend
	make quick

quick:  $(OBJS)
	touch $(MAIN)

clean:
	$(RM) $(OBJS) $(TESTS) *.hi core $(MAIN) profile.* *~ .depend*
	$(RM) *.hcr *.hp t tt fs.out ghc.dv

depend: $(HS_SRCS)
	$(RM) .depend
	$(HC) -M -optdep-f -optdep.depend $(HCFLAGS) $(HS_SRCS)
