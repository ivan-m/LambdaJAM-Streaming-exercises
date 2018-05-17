Exercise 1
==========

The `Stream` type found in the streaming library differs from [Pipes]
and [Conduits] in two very obvious ways:

1. Streams by default take a _Functor_ parameter (which is usually the
   `Of` functor to attach a value type) rather than an explicit value
   type.

2. Pipes and Conduits both have the notion of _upstream_ and
   _downstream_ (or _input_ and _output_) value types/parameters.

Whilst at first the first difference seems odd, we'll see that it
allows for a large amount of flexibility.

However, the second implies a large philosophical difference in how
you typically handle Streams.

In this exercise we will examine both of these differences.

[Pipes]: http://hackage.haskell.org/package/pipes
[Conduits]: http://hackage.haskell.org/package/conduit

Tasks
-----

1. Implement `splitAt`.

2. Port the instances functions from `IStream` to `FStream`; implement
   `chunksOf`.

3.

4.
