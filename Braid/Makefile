# $Id
# Copyright (C) Peter Duncan White, 2003
# Copyright (c) OHSU, 2003
#  Makefile for braid directory

include ../dirs.mk

HC        = ghc
INCS      = $(GRAPH):$(UT):$(RES):$(PLOGIC)
HCFLAGS   = $(DEFS) $(OPTS) -i$(INCS) -syslib data
SEP       = ThreadId.hs     \
            MVarId.hs       \
            MVar.hs         \
            MVarOp.hs       \
            LocalState.hs   \
            ThreadInfo.hs   \
            MVarInfo.hs     \
            Relation.hs     \
            Policy.hs       \
            Thread.hs       \
            LocalThread.hs  \
            BraidLocal.hs   \
            Delay.hs        \
            Thread.hs       \
            BraidState.hs   \
            MVarProc.hs     \
            BraidInternal.hs
HS_IGNORE = $(filter-out TestData.hs TestSupport.hs, $(wildcard Test*.hs))

HS_SRCS   = ${filter-out ${HS_IGNORE}, $(wildcard *.hs)}
OBJS      = $(addsuffix .o,$(basename $(HS_SRCS)))
TESTS     = $(basename $(HS_IGNORE))
MAIN      = target
DOTOS     = $(OBJS) $(UT)/*.o $(GRAPH)/*.o $(RES)/*.o $(PL)/*.o

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

# This is used only in the test program, so glasgow extensions are ok
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

countSep:
	cat $(SEP) | ../loc.prl

clean:
	$(RM) $(OBJS) $(TESTS) *.hi core $(MAIN) profile.* *~ .depend* out*
	$(RM) *.hcr *.hp *.prof *.aux *.ps

depend: $(HS_SRCS)
	$(RM) .depend
	$(HC) -M -optdep-f -optdep.depend $(HCFLAGS) $(HS_SRCS)
