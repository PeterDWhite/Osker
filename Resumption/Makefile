# $Id
# Copyright (C) Peter Duncan White, 2002, 2003
# Copyright (c) OHSU, 2002, 2003
#  Makefile for the Resumption monad directory

#  Get directories involves in the make
include ../dirs.mk

HC        = ghc
INCS      = $(UTILITIES):$(PL)
HCFLAGS   = $(OPTS) $(DEFS) -i$(INCS) -syslib data
HS_IGNORE = Test.hs
HS_SRCS   = ${filter-out ${HS_IGNORE}, $(wildcard *.hs)}
OBJS      = $(addsuffix .o,$(basename $(HS_SRCS)))
TESTS     = $(basename $(HS_IGNORE))
MAIN      = target
DOTOS     = $(OBJS) $(UTILITIES)/*.o

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

quick:  $(OBJS)
	touch $(MAIN)

count:
	cat $(HS_SRCS) | ../loc.prl

clean:
	$(RM) $(OBJS) $(TESTS) *.hi core $(MAIN) profile.* *~ .depend*
	$(RM) *.hcr *.hp

depend: $(HS_SRCS)
	$(RM) .depend
	$(HC) -M -optdep-f -optdep.depend $(HCFLAGS) $(HS_SRCS)
