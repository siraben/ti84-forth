SPASM ?= spasm

all: forth.8xk forth.8xp

forth.8xk: forth-app.asm forth.asm flash-app-runtime.asm inc/ti83plus.inc
	$(SPASM) -N forth-app.asm $@

forth.8xp: forth.asm inc/ti83plus.inc
	$(SPASM) forth.asm $@

clean:
	rm -f forth.8xk forth.8xp forth.lab forth.lst

.PHONY: all clean
