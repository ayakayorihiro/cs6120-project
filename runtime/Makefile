CXXFLAGS = -std=c++17
all: gc brawn_runtime.ll
%.ll: %.cc brawn_runtime.h
	$(CXX) $(CXXFLAGS) -S -emit-llvm $< -o $@
clean:
	rm -rf *.ll *.o *.s
gc:
	make -C deps/gc -j
.PHONY: gc all
