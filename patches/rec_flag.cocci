@@
identifier c;
@@

- c->expr.rec = 1
+ FLAG_SET(c->expr.flags, FLAGS_RECURSIVE)

@@
identifier c;
@@

- c->expr.rec = 0
+ FLAG_CLEAR(c->expr.flags, FLAGS_RECURSIVE)

@@
identifier c;
expression E;
@@

- c->expr.rec = E
+ FLAG_SET_TO(c->expr.flags, FLAGS_RECURSIVE, E)
