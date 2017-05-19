Popr Compiler [![Build Status](https://travis-ci.org/HackerFoo/poprc.svg?branch=master)](https://travis-ci.org/HackerFoo/poprc)
============

This project is to implement a compiler for the Popr (previously Peg) language.  The language has changed somewhat to support efficient compilation and type checking.

Please see [this presentation](http://hackerfoo.com/presentations/ttpl_slides.html) for more information.

Here is a quick description of the language as it is currently. Lines beginning with ":" are user input, the following lines are output from the interpreter.

    : 1 2 +
      3

Popr is a post-fix concatenative language.  `+` is an operator that takes two integers and returns one.  There are several similar operators, with the same meaning as in C: `-`, `*`, `<`, `<=`, `==`, `>`, `>=`.  Booleans are represented as the symbols True and False.

    : 1 2 | 3 +
      4
      5
    : 2 5 | dup 3 >= !
      5

`|` creates alternative execution paths.  `!` takes two values.  If the second argument is False, the it fails, otherwise the first value is returned.  These primitives provide a more general mechanism than if/else, similar to speculative execution, or Prolog inference.

    : [ 1 2 + 3 4 + ] popr
      [ 1 2 + ] 7
    : 1 [ 2 + ] pushl
      [ 1 2 + ]
    : 1 [ 2 + ] pushl popr
      [] 3

`[ ... ]` denotes a quote, which can be used for constructing functions or for aggregating values.  `popr` pops the rightmost element and forces evaluation.  `pushl` takes its first argument and pushes it onto the left of the fragment.  The two primitives can be combined to evaluate a fragment.  Notice that Popr is lazy, and only reduces functions in quotes when required.

    : 1 2 | dup
      1 1
      2 2
    : 1 2 swap
      2 1
    : 1 2 drop
      1

Some other primitives.  `dup` duplicates a value; notice how it respects the constraints introduced by alternatives.  `swap` swaps two values.  `drop` drops a value.

     : 1 2 [3+] dip11
       4 2
     : 1 2 [3] dip12
       1 3 2
     : 1 3 2 [+] dip21
       4 2
     : [1] 2 pushr
       [ 1 2 ]

Some other functions.

License
=======

The license for the Popr Compiler is GPL3.  See LICENSE for more information.
