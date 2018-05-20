Exercise 2
==========

In [Exercise 1](../exercise_1/README.md) you should have had functions
with the following types:


```haskell
splitAt :: (Monad m, Functor f) => Int -> FStream f m r
           -> FStream f m (FStream f m r)

chunksOf :: (Monad m, Functor f) => Int -> FStream f m r
            -> FStream (FStream f m) m r
```

(though it's OK if you used `Of a` instead of the abstract Functor `f` here.)

This ability to have Streams within Streams is very powerful.  In this
exercise we will examine the different ways this can occur and when
and why you may wish to use them.

This will also be the first time we use the actual `streaming`
library.  At time of writing the latest available version is
[0.2.1.0]; later versions may also be suitable (but all documentation
links will be to this version).

[0.2.1.0]: https://hackage.haskell.org/package/streaming-0.2.1.0

Resources
---------

You will probably want to look through the documentation of the
[`Streaming`] and [`Streaming.Prelude`] modules.

[`Streaming`]: https://hackage.haskell.org/package/streaming-0.2.1.0/docs/Streaming.html
[`Streaming.Prelude`]: https://hackage.haskell.org/package/streaming-0.2.1.0/docs/Streaming-Prelude.html

Tasks
-----

1. Streams in result position.

2. Streams in the functor position.

3. Streams in the monadic position.
