.. image:: _static/pic_logo.png
   :align: center
   :width: 250px
   :alt: PIC - Portable Fortran Utility Library

User Guide to PIC
=================

*A portable Fortran utility library for scientific computing*

----

PIC (a Huastec word meaning "otter") is a portable Fortran utility library providing commonly needed features with first-class support across multiple compilers.

Why PIC?
--------

The Fortran ecosystem lacks a standard library that compiles reliably across all compilers. While the Fortran stdlib project aims to fill this gap, it doesn't build on compilers like nvfortran. PIC exists to provide **portable utilities that work everywhere**.

**Key Features:**

- Strings, logging, timers, arrays, sorting, hashing
- Tested on GNU, Intel, NVIDIA HPC, LFortran, Cray, and Flang
- Two build systems: CMake and FPM
- MIT licensed and open source

**Quick Start:**

.. code-block:: fortran

    program main
       use pic, only: pic_print_banner
       implicit none

       call pic_print_banner()

    end program main

Links
-----

- `API Documentation (FORD) <https://jorgeg94.github.io/pic/>`_
- `GitHub Repository <https://github.com/JorgeG94/pic>`_
- `Examples Repository <https://github.com/JorgeG94/pic_examples>`_

.. toctree::
   :maxdepth: 2
   :caption: User Guide

   installation
   getting_started
   features

.. toctree::
   :maxdepth: 2
   :caption: Developer Guide

   contributing
   compiler_portability

Indices and tables
==================

* :ref:`genindex`
* :ref:`search`
