# $Id
# Copyright (C) Peter Duncan White, 2001, 2002, 2003
# Copyright (c) OHSU, 2001, 2002, 2003
#  Makefile for the test support modules

#  Get directories involves in the make
include ../dirs.mk

HC        = ghc
INCS      = $(UT):$(RES):$(BR):$(UP)
HCFLAGS   = -cpp -O2 $(DEFS) -i$(INCS) $(OPTS) -Wall -syslib concurrent -syslib data
HS_IGNORE = Test.hs
HS_SRCS   = ${filter-out ${HS_IGNORE}, $(wildcard *.hs)}
OBJS      = $(addsuffix .o,$(basename $(HS_SRCS)))
TESTS     = $(basename $(HS_IGNORE))
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

%: %.hs
	ghc -o $@ $(HCFLAGS) $< $(DOTOS)
	rm -f $@.o
	rm -f $@.hi

$(MAIN): depend
	make depend
	make quick

DOTOS = $(OBJS)

quick:  $(OBJS)
	touch $(MAIN)

clean:
	$(RM) $(OBJS) $(TESTS) *.hi core $(MAIN) profile.* *~ .depend*
	$(RM) *.hcr *.hp

depend: $(HS_SRCS)
	$(RM) .depend
	$(HC) -M -optdep-f -optdep.depend $(HCFLAGS) $(HS_SRCS)
