---
title: Design
---


## Design philosophy

The idea of this library was inspired a bit by a very cool talk Chris Lattner gave
at some conference. In his talk, he mentions how the LLVM project just grew in
many directions thanks to how the design of the project was done. I.e. the fact
that most of the components of the LLVM are in the `lib` subdirectory.

If you've never compiled the LLVM I suggest you do it, you'll see that you can
enable different compoenents depending on your needs. Rarely does one need to build
_everything_ in it.

The same idea will be applied to PIC. I want to provide a set of components
that can be used independently of each other. There will be of course, a core library
that will provide the most basic functionality. For example, everything at the very bottom depends
on the `pic_types.F90` module, so you cannot really use PIC without it.

So following this concept, PIC will create a series of libraries that are based
upon the core library, i.e. the very basic functionality that will always be built
no matter what project you select on top.

This idea is more important if you're using the CMake build system, since this
will need to be configured when building from source. Otherwise, if you are using
the Fortrn package manager and using `pic` as a dependency in your project, the FPM
will only builds the modules/components that you actually include in your project.

I find this very neat.

At this point, the core library is everything, I haven't refactored the code to be
built into separate libraries, however I already have the modularity to not build
certain bits, like BLAS, or MPI. I envision the following structure

```
libpic_core
libpic_blas
libpic_mpi
libpic_gpu
```

And PIC will be able to build all of these with `default_int` set to either `int32` or `int64`, depending on the needs of the user.
Naturally, the MPI module will only use `int32` as the default integer type, since MPI (`mpi_f08`) does not support `int64` in its API.

## Design goals

I hope that this library will be useful for many people, mostly as a fast and efficient way to
write parallel code in Fortran. I also hope that it will be a good example of how
to write (modern) Fortran code that is modular, extensible, and easy to use.

This is also my attempt into showcasing that Fortran is not a boring old language that only has
a relevance in legacy code.

I want to show that Fortran can be used to write modern, high-performance code that is easy to read and maintain.
