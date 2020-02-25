Popr Compiler [![Build Status](https://travis-ci.org/HackerFoo/poprc.svg?branch=master)](https://travis-ci.org/HackerFoo/poprc)
============

This project implements a compiler for the Popr language.

Please see [this presentation](http://hackerfoo.com/presentations/ttpl_slides.html) for more information, and the [tutorial](http://www.hackerfoo.com/posts/popr-tutorial-0-dot-machines.html) to understand the language semantics.

More examples can be found in [lib.ppr](https://github.com/HackerFoo/poprc/blob/master/lib.ppr) and [tests.ppr](https://github.com/HackerFoo/poprc/blob/master/tests.ppr).

You can try the compiler online using the [PoprC web version](http://hackerfoo.com/eval.html).

PoprC targets both C and Verilog. For example, the familiar Fibonacci function:

    fib: [dup 1 <= !] [dup 1- dup 1- fib swap fib + swap 1 > !] | pushl head

can be compiled to Verilog and synthesized or simulated:

![fib wave](pic/fib_wave.png)

Note the stack pointer (sp). Recursion is fully supported, but must be bounded.

License
=======

The license for the Popr Compiler is GPL3.  See LICENSE for more information.
