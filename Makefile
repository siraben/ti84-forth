SPASM ?= spasm

all: forth.8xp

forth.8xp: forth.asm inc/ti83plus.inc
	$(SPASM) forth.asm $@
