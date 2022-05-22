CXXFLAGS = -std=c++17
all: gc brawn_runtime.ll
%.ll: %.cc brawn_runtime.h
	$(CXX) $(CXXFLAGS) -S -emit-llvm $< -o $@
clean:
	rm -rf *.ll *.o *.s
	make -C deps/gc clean
gc:
	test -f deps/gc/configure || (cd deps/gc; ./autogen.sh; ./configure --enable-cplusplus --enable-static --disable-shared)
	make -C deps/gc -j
.PHONY: gc all
