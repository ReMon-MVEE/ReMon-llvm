# ReMon LLVM

## Introduction
This repository hosts the LLVM part of the ReMon atomicize compiler. 
This compiler can prepare multithreaded programs for running inside an MVEE. 
To do this, it intervenes at four different stages of the building process:

### Front-end (clang)
The compiler enforces strict typing discipline by:
- refusing to compile code that discards the `_Atomic` or `volatile` type-qualifier through pointer type casts;
- refusing to compile code that contains inline asm statements with atomic operations AND control-flow instructions;
- refusing to compile code that passes less or more than one `_Atomic` or `volatile` qualified operand to inline asm statements containing atomic operations. Passing the same qualified operand multiple times (e.g., once as an input and once as an output operand) is allowed, however!

During clang's code-generation stage, the atomicize compiler translates all operations affecting volatile variables into atomic operations.

All of these steps are disabled for variables annotated with the `__attribute__((nonsync))` attribute.

### Back-end (llvm)
The compiler instruments all atomic (and volatile) operations by adding a call to `mvee_atomic_preop_trampoline` before and a call to `mvee_atomic_postop_trampoline` after every atomic (or volatile) instruction.
A pointer to atomic (or volatile) variable affected by the instruction is passed to the `mvee_atomic_preop_trampoline` function.

### Linking
The compiler links the instrumented binary to libclang_rt.sync-<arch>.so. 
This library implements the `mvee_atomic_preop_trampoline` and `mvee_atomic_postop_trampoline` functions.

## Installation

### Disclaimer

Please keep in mind that this compiler has only been tested on Ubuntu 14.04!

### Getting the source code

Set your llvm tree up as follows:
```
git clone git@github.com:securesystemslab/ReMon-llvm.git llvm
git clone git@github.com:securesystemslab/ReMon-clang.git llvm/tools/clang
git clone git@github.com:securesystemslab/ReMon-compiler-rt.git llvm/projects/compiler-rt
```

### Building the compiler

```
mkdir -p llvm/build-tree && cd llvm/build-tree
cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_TARGETS_TO_BUILD=X86 ..
make -j 8
```

## Using the compiler

It is fairly straightforward. Just pass `-fatomicize` as a linker and compiler flag. To build nginx, for example, you would do:

```
./configure --with-threads --with-cc=/path/to/llvm/build-tree/bin/clang --with-cc-opt="-fatomicize" --with-ld-opt="-fatomicize"
make -j 4
```

Some projects (e.g. Apache) use a compiler/linker wrapper called libtool.
Libtool (quite annoyingly) strips any flags it doesn't recognize from the CFLAGS, CXXFLAGS and LDFLAGS. 
To trick libtool into accepting the "-fatomicize" flag, you can just pass it as a part of the compiler name when you call configure.
You can do that as follows:

```
CC="/path/to/llvm/build-tree/bin/clang -fatomicize" CXX="/path/to/llvm/build-tree/bin/clang -fatomicize" LD="/path/to/llvm/build-tree/bin/clang -fatomicize" ./configure
make -j 4
```

Keep in mind that the resulting binary will be linked to libclang_rt.sync-<arch>.so. 
If you want to run the binary outside ReMon, you'll have to make sure that libclang_rt.sync is in your LD_LIBRARY_PATH.
If you want to run the binary in ReMon, you don't have to touch the LD_LIBRARY_PATH as ReMon will do it for you.

## Publications

[Taming Parallelism in a Multi-Variant Execution Environment](http://ics.uci.edu/~stijnv/Papers/eurosys17-parallelism.pdf)
Stijn Volckaert, Bart Coppens, Bjorn De Sutter, Koen De Bosschere, Per Larsen, and Michael Franz.
In 12th European Conference on Computer Systems (EuroSys'17). ACM, 2017.
To appear.