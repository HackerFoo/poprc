Peg Compiler
============

This project is to implement a compiler for the Peg language.  The language has changed somewhat to support efficient compilation and type checking.

Here is a quick description of the language as it is currently:

    1 2 +   -->   3

Peg is a postfix concatenative language.  `+` is an operator that takes two integers and returns one.  There are several similar operators, with the same meaning as in C: `-`, `*`, `<`, `<=`, `==`, `>`, `>=`.  Booleans are currently represented as integers, 1 for true, 0 for false.

    1 2 | 3 +     -->   [ { 4 | 5 } ]
    2 5 | 2 - !   -->   [ 3 ]

`|` creates alternative execution paths.  `!` takes one integer.  The execution path is pruned if this argument is 0.  These primitives provide a more general mechanism than if/else, similar to speculative execution, or Prolog inference.

    [ 1 2 + 3 4 + ] popr   -->   [ [ 1 2 + ] 7 ]
    1 [ 2 + ] pushl        -->   [ [ 1 2 + ] ]
    1 [ 2 + ] pushl popr   -->   [ [] 3 ]

`[ ... ]` denote a fragment, which can be used for constructing functions or for aggregating values.  `popr` pops the rightmost element and forces evaluation.  `pushl` takes its first argument and pushes it onto the left of the fragment.  The two primitives can be combined to evaluate a fragment.  Notice that Peg is lazy, and only reduces fragments when required.

    1 2 | dup   -->   { [ 1 1 ] | [ 2 2 ] }
    1 2 swap    -->   [ 2 1 ]
    1 2 drop    -->   [ 1 ]
    1 2 | cut   -->   [ 1 ]

Some other primitives.  `dup` duplicates a value; notice how it respects the constraints introduced by alternatives.  `swap` swaps two values.  `drop` drops a value.  `cut` executes only one alternative.  This should only be used to hint that only one alternative would succeed, rather than to prune otherwise successful execution paths.  The type checker will eventually check this.

     [1 2 + 3 4 + force] popr   -->   [ [ 3 ] 7 ]

`force` ties the execution of its arguments together, so that if one is reduced, the other will be as well.

     1 2 [3+] dip11    -->   [ 4 2 ]
     1 2 [3] dip12     -->   [ 1 3 2 ]
     1 3 2 [+] dip21   -->   [ 4 2 ]
     [1] 2 pushr       -->   [ [ 1 2 ] ]

Some other functions.

Try it out!

<iframe src="http://hackerfoo.com.nyud.net/peg" width="800" height="600"></iframe>

License
=======

The license for the Peg Compiler is GPL3.  See LICENSE for more information.
