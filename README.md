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

[An XKCD strip](http://xkcd.com/1313/) introduced the idea of Regex
Golf, trying to produce small regexes that accept one list but not the
other.

[Peter Norvig got involved](http://nbviewer.ipython.org/url/norvig.com/ipython/xkcd1313.ipynb),
and solving it looks NP-hard, due to being rather like set cover.

On the more practical hand, [this site](http://regex.alf.nu/) lets you
play Regex Golf manually. Many work friends did so.

[This level](http://regex.alf.nu/9) of the site wants you to match
multiples of three.

[This page](http://quaxio.com/triple/) shows how to solve the problem.

That's one case, solved manually. I can do better than that! Hence
this program. :)

As stated above, it's a quick hack. It was built quickly, abondoned on
my hard drive, then briefly polished and dumped on the internet as it
kinda seemed a shame not to do so.

It's in Haskell, as Haskell was my day-job language for a few years,
and it's still my preferred language for rapid development, as it's
got much of the expressiveness of scripting languages, but with the
benefits of static typing.

It's not production-quality code, it's a hack. Error-checking is
minimal. There's no test suite. I think you get the idea. Have fun!

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
