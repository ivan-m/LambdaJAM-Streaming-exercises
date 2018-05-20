Exercise 3
==========

It has hopefully been apparent via the previous exercises that
`Stream`s can be considered as a more powerful variant of Haskell
lists with interleaved monadic effects.

In particular, the [`Streaming.Prelude`] module contains most of
`Data.List` (though it is missing some functions, such as `tail`).
There is one major difference though: there are no textual-specific
functions (such as `lines`).

This exercise aims at emphasising that by having you port list-ful
code to using `Stream`s.

Resources
---------

The [`Streaming.Prelude`] module should contain all you need, but the
[`Streaming`] module may be helpful..

[`Streaming`]: https://hackage.haskell.org/package/streaming-0.2.1.0/docs/Streaming.html
[`Streaming.Prelude`]: https://hackage.haskell.org/package/streaming-0.2.1.0/docs/Streaming-Prelude.html

Tasks
-----

1. Implement some relatively simple `String`-like `Stream` functions.

2. Implement `titleCase`.

3. Port list-based Fibonacci to Stream-based.
