---
title: Contributing
---

## Contributing to PIC

All contributions to PIC are welcome! If you have an idea for a new feature, a bug fix, or an improvement, please open an issue or submit a pull request.

PIC is an open source project licensed under the MIT License, so you can use it in your own projects without any restrictions. If you use PIC in your project and your wrote a paper with it, please consider citing it by including a link to the repository in your paper.

All code that you contribute to PIC will be licensed under the MIT license, so please make sure that you are comfortable with this before submitting your changes.


## How to contribute

The following guidelines will help you contribute effectively:

- **Fork the repository**: Create your own fork of the PIC repository on GitHub.
- **Create a new branch**: Always create a new branch for your changes. This keeps your work organized and makes it easier to manage pull requests. I don't really care about the branch name, but I suggest you abide by `[type]/[description]` format; where `type` is one of `feat`, `fix`, `docs`, `chore`, and `experiment`.
- **Write tests**: If you are adding new features or fixing bugs, please include tests to ensure that your changes work as expected.
- **Follow the coding style**: Please adhere to the coding style used in the project. This includes naming conventions, indentation, and commenting.
- **Document your changes**: Update the documentation to reflect any changes you make. This includes updating the README, comments in the code, and any relevant documentation files.
- **Run tests**: Before submitting your changes, make sure to run the tests to ensure that everything works as expected. The CI should catch any compiler issues.
- **Submit a pull request**: Once you have made your changes and tested them, submit a pull request to the main repository. Provide a clear description of your changes and why they are needed.


The most important thing in the code is that we are aiming to support `int32` and `int64` as the default integer types. This is why
we use the `default_int` parameter in the `pic_types.F90` module. When you are writing new code, please ensure that it is compatible with both integer types by always using `integer(default_int)` instead of `integer` or `integer(kind=4)` or `integer(kind=8)`.

### On the use of LLMs, AI thingies, etc.

I do not care. As long as your code is tested, works, and is well documented I could not care less if you used an AI to write it.

Can I copy paste code from PIC into ChatGPT, Claude, or whatever AI I use? Yes, you can. I don't care. I do not consider this plagiarism, since the code is open source and you are free to use it as you wish.

## Documenting your code

We use Ford to generate the documentation. The idea is to have coverage of almost every function in the code, except those that
can be self explanatory or if they are `module procedure` inside an `interface` block. For example:

```
  interface fill
  !! fill provides a generic interface to assing a value
  !! alpha of types (int32, int64, sp, dp) as defined in pic_types.F90
  !! The inteface supports filling 1d and 2d arrays of the specified
  !! variables
  !!
  !! Usage: call fill(array, value, [optional] threaded)
  !!
  !! This subroutine is threaded for performance purposes if threaded is set to .true.
  !!
    module procedure fill_vector_int32
    module procedure fill_vector_int64
    module procedure fill_vector_sp
    module procedure fill_vector_dp
    module procedure fill_matrix_int32
    module procedure fill_matrix_int64
    module procedure fill_matrix_sp
    module procedure fill_matrix_dp
  end interface
```


There's really no need to go document each `fill_vector_*` or `fill_matrix_*` procedure, since the interface already provides
everything we need to know about the procedures within the `fill` interface. Documenting each of these would mean to just
write `fills a vector of type int32 with the value alpha, uses threads if enabled` which is redundant and not really useful.

## Code style

We mostly follow the best practices outlined in the [Fortran Best Practices](https://fortran-lang.org/learn/best-practices/) guide.

We have a pre-commit hook that will run `fprettify` to format the code before committing, this will ensure that the formatting is consistent across the codebase.

To install pre-commit do:

```bash
python3 -m pip install pre-commit
pre-commit install
```

You can also run `pre-commit run --all-files` to format all the files in the repository.

### Module naming convention

All our of modules are named using the `pic_` prefix, followed by the name of the module. For example, the module that provides the basic types is called `pic_types`, and the module that provides the BLAS functionality is called `pic_blas_interfaces`. MOst of the time the module name matches the name of the file, if you find an instance where this is not the case, please open an issue or submit a pull request to fix it.

Based on the `file = module_name` convention, there should only be one module per file.

We follow the convention that the `end module` statement should contain the name of the module.

### Derived type naming convention

All derived types should be named using the `pic_` prefix, followed by the name of the type. For example, the type that represents a vector is called `pic_vector_type`, and the type that represents a matrix is called `pic_matrix_type`.

### Function and subroutine naming convention

I really don't care about naming conventions for functions or subroutines, I only suggest that you make the name descriptive and meaningful. Also,
using interfaces to hide the name of the subroutine inside the module is a good practice, since it allows you to change the implementation without affecting the users of the module.

For example, in `pic_timer.F90` we have the following interface:



```fortran
   type :: pic_timer_type
    !! derived type for a timer, contains the start, stop, and count variables
    !! can work with or without omp
      private
      real(dp) :: start_time = 0.0_dp
      real(dp) :: stop_time = 0.0_dp
      real(dp) :: walltime
      logical :: is_running = .false.
      integer(default_int) :: start_count = 0_default_int
      integer(default_int) :: stop_count = 0_default_int
      integer(default_int) :: count_rate = 1_default_int
   contains
      procedure :: start => timer_start
      procedure :: stop => timer_stop
      procedure :: print_time => timer_print_time
      procedure :: get_elapsed_time => timer_get_elapsed_time
   end type pic_timer_type
```


You can see that the `start` procedure is mapped to `timer_start`, this way it is more intuitive for people to use `my_timer%start()` instead of `my_timer%timer_start()`. This is a good practice to follow, since it makes the code more readable and easier to use.

We follow the same convention for functions and subroutines, as we do for modules. This is, the `end function` or `end subroutine` statement should contain the name of the function or subroutine.

## Code reviews

Code reviews are an essential part of the contribution process. When you submit a pull request, it will be reviewed by the maintainers of the project. They will provide feedback on your code, suggest improvements, and ensure that it meets the project's standards.

Please be open to feedback and willing to make changes to your code based on the review. The goal is to improve the quality of the code and ensure that it is maintainable in the long run.

If you are new to contributing to open source projects, don't be discouraged by the review process. It is a learning experience, and the maintainers are here to help you improve your code and understand the project better.

If you feel connected to the project and want to help out with reviews, please let me know. I would be happy to invite as many people as possible to the main repository.
