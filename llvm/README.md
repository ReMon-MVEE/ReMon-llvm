# ReMon LLVM

## Introduction
This repository hosts the LLVM part of the ReMon atomicize compiler.

This compiler does three things:
- It refuses to compile code that discards the `_Atomic` type-qualifier through type casts.
- It refuses to compile code that uses `_Atomic` variables in inline assembly.
- It can instrument atomic operations by adding a call to `mvee_atomic_preop` before and a call to `mvee_atomic_postop` after every atomic instruction.
- It can link binaries to libclang_rt.sync-<arch>.so. This library implements the preop/postop interface.

## Installation

### Disclaimer

Please keep in mind that:
- This compiler has only been tested on Ubuntu 14.04!
- You should ***NOT*** use this compiler as a replacement for your system compiler as it is ***NOT*** C-compliant. It will, for example, refuse to compile code that casts a pointer to an `_Atomic` variable to `void*` even though this is explicitly allowed by the C standard.

### Getting the source code

Set your llvm tree up as follows:
```git clone git@github.com:stijn-volckaert/ReMon-llvm.git llvm
git clone git@github.com:stijn-volckaert/ReMon-clang.git llvm/projects/clang
git clone git@github.com:stijn-volckaert/ReMon-compiler-rt.git llvm/projects/compiler-rt```

### Building the compiler

`mkdir -p llvm/build-tree && cd llvm/build-tree`
`cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_TARGETS_TO_BUILD=X86 ..`
`make -j 8`

## Using the compiler

It is fairly straightforward. Just pass `-fatomicize` as a linker and compiler flag. To build nginx, for example, you would do:

```./configure --with-threads --with-cc=/path/to/llvm/build-tree/bin/clang --with-cc-opt="-fatomicize" --with-ld-opt="-fatomicize"
make -j 4```

Keep in mind that the resulting binary will be linked to libclang_rt.sync-<arch>.so. 
If you want to run the binary outside ReMon, you'll have to make sure that it's in your LD_LIBRARY_PATH.
If you want to run the binary in ReMon, you don't have to touch the LD_LIBRARY_PATH as ReMon will do it for you.

