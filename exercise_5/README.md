Exercise 5
==========

This is a more free-form exercise.

In this directory there is a [CSV file](languages.csv) containing a
list of programming languages obtained from
[Wikipedia](https://en.wikipedia.org/wiki/Comparison_of_programming_languages).

Resources
---------

As before:

* [`Streaming`]

* [`Streaming.Prelude`]

* [`streaming-with`]

For CSV parsing, see [`streaming-cassava`] (version 0.1.0.1 at time of
writing).

, the [`Streaming`] and [`Streaming.Prelude`] modules will be
of use.  In addition, we will be using the [`streaming-with`] and
[`streaming-bytestring`] (versions 0.2.1.1 and 0.1.6 respectively at
time of writing) libraries.

[`Streaming`]: https://hackage.haskell.org/package/streaming-0.2.1.0/docs/Streaming.html
[`Streaming.Prelude`]: https://hackage.haskell.org/package/streaming-0.2.1.0/docs/Streaming-Prelude.html
[`streaming-with`]: http://hackage.haskell.org/package/streaming-with-0.2.1.1
[`streaming-cassava`]: https://hackage.haskell.org/package/streaming-cassava-0.1.0.1

Tasks
-----

Your final exam is to:

1. Read in the [CSV file] (note: if using `stack` you will need to
   specify the directory as well!).

2. Write all Functional programming languages to another file.

### Extensions

3. For every explicit paradigm (i.e. ignore the "Other paradigm(s)"
   column), write out those languages that support it to its own file.

    Remember, some languages are multi-paradigm!

4. Can you do anything else with the data? Print number of supported
   paradigms for each language?  Sort it somehow?
