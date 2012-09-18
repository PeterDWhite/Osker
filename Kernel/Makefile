# $Id
# Copyright (C) Peter Duncan White, 2001, 2002, 2003
# Copyright (c) OHSU, 2001, 2002, 2003
#  Makefile for the Osker Kernel
#  Specifically, for the kernel implementation layer of the Osker

#  Get directories involves in the make
include ../dirs.mk

MAIN	= target

$(MAIN):
	cd $(GLOBAL)     ; make $(DEFS)
	cd $(ACTOR)      ; make $(DEFS)
	cd $(KPROC)      ; make $(DEFS)
	cd $(KDOM)       ; make $(DEFS)
	cd $(MSG)        ; make $(DEFS)
	cd $(KPLAT)      ; make $(DEFS)
	cd $(SYS2)       ; make $(DEFS)
	cd $(DDS)        ; make $(DEFS)
	cd $(TCK)        ; make $(DEFS)
	cd $(FS)         ; make $(DEFS)
	cd $(KERNELCORE) ; make $(DEFS)

clean:
	rm -f *~
	cd $(GLOBAL)     ; make clean
	cd $(ACTOR)      ; make clean
	cd $(KPROC)      ; make clean
	cd $(MSG)        ; make clean
	cd $(KDOM)       ; make clean
	cd $(KPLAT)      ; make clean
	cd $(SYS2)       ; make clean
	cd $(DDS)        ; make clean
	cd $(TCK)        ; make clean
	cd $(FS)         ; make clean
	cd $(KERNELCORE) ; make clean
