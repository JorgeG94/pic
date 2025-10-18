---
title: About PIC
---

<img src="../../images/pic_logo.png" alt="Otter coding logo" title="Project logo" width="200">

## Reasoning behind PIC

In my relatively short career as a programmer I have used multiple languages: C, C++, Python, Julia, Bash, and of course, Fortran.

During this time I have explored multiple avenues within each of the languages and have written quite a bit of code in most of them. Probably
the one I've written the most code in is C++, since I was one of the main contributors the GAMESS-ECP project that saw the
creation of GMS-HPC and the initial lines of code of what would become EXESS.

While I was a postdoc at the Barca group I had the joy of meeting some of the best computer scientists I've ever met. I quickly realized
that I was John Snow, I didn't know anything.

It was the first time that I had the opportunity to colaborate closely with people that were trained in computer science and software engineering.

Before that I lived in the academic programming world, which is mostly a "WRITE CODE, PUBLISH PAPER, GET GRANT" type of mindset. If a code
works, nice, you go ahead and publish a paper on it. Is the code good? Most of the times the answer to this is: no.

I was fortunate enough to be a part of the rewriting of the EXESS codebase. Lead by, hopefully soon to be, Dr. Ryan Stocks we rewrote
the entire thing using some very cool features of the C++ language and tightly coupled it with MPI, CUDA/HIP, and ran some of the largest and and most performant simulations ever done.

We even won a Gordon Bell prize in 2024 because of this!

In the end this was a gigantic team effort which wouldn't have been possible without the contributions done by the entire team and the money provided by Dr Barca's company QDX. Without this it would have been impossible to rewrite the entire code the way we did.

After leaving my position as a postdoc at the Barca group I joined the National Computational Infrastructure (NCI) in Canberra where
I could put to use all of my HPC and software engineering knowledge to test in other domains outside of computational quanutm chemistry.

In my last year or so of my postdoc I took up a big interest in the Fortran programming language. I had used it before during my undergrad and early PhD but the ECP project had pushed me to full C++ and CUDA for GPU support. I discovered the beautiful community that is the Fortran Discourse and the associated projects people associated to it have undertaken. Examples are the Fortran Package Manager, the Fortran Standard Library and the LFortran compiler.

I found a community of people that I hadn't found in the C/C++ world and this drove me to start exploring the language a bit more.

### Why Fortran

Fortran is an old language and with age comes lots of baggage. A lot of Fortran's perceived baggage is due to legacy codes that use
old language constructs that are no longer in the standard or just simply bad academic code.

I somehow dislike the idea of the existence of Modern Fortran, but since there is also Modern C++ I wonder if my dislike is not reasonble.

Modern Fortran or from now on just "Fortran" is whatever the language became since the release of Fortran 90 and beyond.

People usually associate Fortran with code that looks like:

```
       PROGRAM MAIN
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)

       REAL*8 HTY,FTY,HUY,RUY

       CALL MTRBR(ABC,BAC, CDF, EFG, DFGE, ASY)

       IF ( VAR.EQ.TRUE ) THEN
        GOTO 500
       ENDIF

       ...

   500 PRINT *, "YOU FOOL!"

       END PROGRAM MAIN

```

The ugly all caps; undescriptive, short variable names; the use of GOTO statements, and of course, the code starting in the 7 column.

There is a lot of code that looks like this yes, however, new Fortran does not look like this at all. This is the famed transition
from fixed to free format Fortran which lets one write code in whichever column you want to write.

New Fortran looks quite differnt and I'd dare even say similar to other language, like Julia maybe. For example, this snippet of code from PIC itself:


```
   subroutine fill_vector_int64(vector, alpha, threaded)
      integer(int64), intent(inout) :: vector(:)
      integer(int64), intent(in)    :: alpha
      logical, intent(in), optional :: threaded
      logical :: use_threads
      integer(default_int) :: i

      if (present(threaded)) then
         use_threads = threaded
      else
         use_threads = use_threaded_default
      end if
      if (use_threads) then
         !$omp parallel do collapse(1) private(i)
         do i = 1, size(vector, 1)
            vector(i) = alpha
         end do
         !$omp end parallel do
      else
         vector = alpha
      end if

   end subroutine fill_vector_int64
```

There is nothing "ugly" to look at here. Everything is nice lower caps with nice spacing. Variable names mean something
to anyone that can speak english and I even showcase the use of OMP parallelism.


One thing that you might comment on here "Oh, ew, you have to declare everything at the start like in C89". And yeah,
I see that comment a lot. HOWEVER. This is not the case anymore. I just did it like this here because the subroutine is rather small.

Fortran has introduced the concept of `blocks` which are used to determine scope in a program, very similar to the `{}` in C/C++. For example, the above code could be rewritten as:

```
   subroutine fill_vector_int64(vector, alpha, threaded)
      integer(int64), intent(inout) :: vector(:)
      integer(int64), intent(in)    :: alpha
      logical, intent(in), optional :: threaded
      logical :: use_threads

      if (present(threaded)) then
         use_threads = threaded
      else
         use_threads = use_threaded_default
      end if
      if (use_threads) then
      loop: block
        integer(default_int) :: i
         !$omp parallel do collapse(1) private(i)
         do i = 1, size(vector, 1)
            vector(i) = alpha
         end do
         !$omp end parallel do
      end block loop
      else
         vector = alpha
      end if

   end subroutine fill_vector_int64
```

And now we've limited the scope of the `i` variable in the program. Blocks _can_ be named or also omitted. I.e. you could ommit the name `loop:` and you'd still be fine. You can nest blocks in blocks, they just delimit the scope of variables.

Now you might wonder, why do I need to say what size of `int`? why am I using a `default_int` size for my integer. Well, this is one of
the things in PIC that had to be done in order for the code to be interfaceable with legacy Fortran applications.

You see, in the old days, codes that wanted to use `int64` or `double precision`, i.e. `fp64`, had to use something similar to
`real(8) :: my_var` this meant the number of bits used to represent the variable. People back then were lazy and the computers
did not help for this. You could tell the compiler to make all `integer` or `real` variables a default of `int64` and `double precision`.

This way all of your integers and reals would always be the precision you wanted them to be. This is unsurprisingly a very bad design decision.

This makes it such that if any code wants to interface with your code the sizes of integers and reals need to match. This becomes
extraordinarily painful if interfacing with C/C++ codes.

Therefore, to avoid using this compiler flags I've set up PIC so that the default integer size can be toggled at compile time from within the program itself - not via the compiler.



But I am getting distracted now.

*Why Fortran?*

Well one: out of spite. The C++ people seem to believe that you cannot achieve the same level and portability as a C++ code can. I
don't believe this but I cannot just say it, I need to show it.

I believe that Fortran can provide an easy entrance and avenue to High Performance Scientific Computing without having to be bogged
down by all of the program design choices that need to be taken if a code is written in C++.

In the end, it is all code. Certain things are easier in Fortran, certain things are easier in C++. I'd argue that it is easier for
scientists, who have little time to think about "How to write good code", to write decent code in Fortran than in C++.

C++ is an extremely powerful language, beyond my comprehension; however, the language is vast, a never ending horizon without mountains in sight. There is simply too much and for newcomers this is overwhelming. I experienced it and continue to do so, even after 8 years of programming in C++.

Fortran is also extremely powerful - but also, not so vast as C++. This is due to the lack of standard library that is distributed with every compiler. Fortran is making a Standard Library but it is in its early stages.

This is also a pain point in Fortran. There are many things in the standard library that are just good and make many tasks simple. The idea of the `#include <iostream>` and you get extremely powerful `I/O` capabilities. Also `std::vector`, `#include <algorithm>` etc. they provide a developer with very well built implementations of very simple (mundane tasks).

For example, in Fortran there is no `sort` function that provides similar functionality to the ones in C++. The Fortran standard library is aiming to fix this, and they are working on it right now.

However, there are many other functionalities that need to be covered and implemented for all types of programs. This is what drove me
to create PIC.

PIC is intended to be a set of functions and subroutines that facilitate developing code aimed for High Performance Computing for Scientific Applications. For example, weather simulations, finite element methods, computational quanutm chemistry, molecular dynamics, etc. The idea of PIC is to provide seamless interfaces to MPI, BLAS to allow people to think about their science instead of the code.

The aim is to maybe "look pythonic in nature", but probably closer to Julia.

For example, being able to do `call pic_gemm(A,B,C)` and have PIC decide the bounds of the arrays and if the operations happens on the CPU or the GPU.

PIC aims to provide a stable development platform for people that want to write high performance code for modern hardware architectures.
