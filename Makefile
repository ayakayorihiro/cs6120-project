all: runtime
	dune build
runtime:
	make -C runtime
clean:
	make -C runtime clean
	dune clean
.PHONY: all runtime

