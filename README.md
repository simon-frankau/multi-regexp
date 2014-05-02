# MultiRegExp - A regular expression multiple acceptor

MultiRegExp generates regular expressions which will only accept the
multiples of a given number. For example, it can generate a regexp
which only accepts multiples of three.

Actually, it can do more than that. It can generate regexps which
match numbers which are equal to _n_ modulo _m_, in base _b_.

It does not do this efficiently! It's very much a toy, very much
unoptimised, and the output scales horrifically with the input
parameters.

## Background

FIXME: To expand!

## Building

ghc --make MultiRegExp

## Using

"./MultiRegExp _b_ _m_ _n_" generates a regexp that processes numbers
shown in base _b_, matching those that are equal to _n_ mod _m_.

A very simple example of the non-optimising behaviour of this code can
be seen if you try "MultiRegExp _b_ 2 0", where _b_ is even. The
simplest result is effectively ".*[0246...]", but the code misses that
case!

## Things that might be fun to do

"MultiRegExp 10 3 0" is the case that launched writing this code.

You can try scaling the first two parameters, and watch the size of
the output scale hugely. Plot it, try to get a bound, that kind of
thing.

I was going to try this myself, but I never got around to it (trivial
though it is)!
