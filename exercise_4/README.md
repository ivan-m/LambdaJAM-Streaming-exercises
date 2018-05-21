Exercise 4
==========

The motivation for stream processing libraries is, of course, the
[known] [problems] with Lazy I/O.  In short: as standard I/O in
Haskell (including using lazy `ByteString`s, etc.) uses functions like
`unsafeInterleaveIO` under the hood, it is almost impossible to reason
about ordering and ensure required effects occur in the required
order.

[known]: https://www.reddit.com/r/haskell/comments/1e8k3k/three_examples_of_problems_with_lazy_io/c9xyxxy
[problems]: https://stackoverflow.com/questions/5892653/whats-so-bad-about-lazy-i-o

(Please note: it _is_ possible to write production-grade Haskell I/O
code using lazy I/O; but if you do anything except read in one file
lazily and write it back out to another file it can be very difficult
to manage.)

As such, let's examine how to do simple I/O using the streaming
ecosystem and higher levels of resource management.

Resources
---------

As before, the [`Streaming`] and [`Streaming.Prelude`] modules will be
of use.  In addition, we will be using the [`streaming-with`] and
[`streaming-bytestring`] (versions 0.2.1.1 and 0.1.6 respectively at
time of writing) libraries.

[`Streaming`]: https://hackage.haskell.org/package/streaming-0.2.1.0/docs/Streaming.html
[`Streaming.Prelude`]: https://hackage.haskell.org/package/streaming-0.2.1.0/docs/Streaming-Prelude.html
[`streaming-with`]: http://hackage.haskell.org/package/streaming-with-0.2.1.1
[`streaming-bytestring`]: http://hackage.haskell.org/package/streaming-bytestring-0.1.6

Tasks
-----
