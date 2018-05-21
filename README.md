I Streamed a Stream
===================

These are the exercises for the [Stream Processing Code
Jam](http://lambdajam.yowconference.com.au/proposal/?id=6197) at the
[2018 YOW! Lambda Jam](https://confengine.com/yow-lambda-jam-2018)
conference.

Understanding the exercises
---------------------------

Each exercise contains a `README.md` that you should read to gain
background understanding of the tasks involved.

However, the actual details are found in the actual source files.
Each exercise has its Haskell source file (within
`LambdaJAM/Streaming/`) with detailed discussion, instructions, etc.

Why is the first exercise numbered "0"?
---------------------------------------

1. [Dijkstra](https://www.cs.utexas.edu/users/EWD/transcriptions/EWD08xx/EWD831.html). QED.

2. More realistically: because if you're comfortable enough with
   Haskell data structures, etc. you may wish to skip this one.

    It's not like I spent any time on it...

How to run these exercises
--------------------------

You have the option of three build tool options:

1. `stack`: from the root directory, run `stack build` to get all
   dependencies, etc. built.

    You can then do `stack ghci exercise0:lib`, etc. to load each
    exercise's file.

    Note that for `exercise_5`, you may need to specify a filename of
    `"exercise_5/languages.csv"`.

2. `cabal-install`: you will need to consider each exercise in
   isolation.  For example:


    ```bash
    ivan LambdaJAM-Streaming-exercises $cd exercise_0/
    ivan exercise_0 $cabal sandbox init
    Writing a default package environment file to
    /home/ivan/Haskell/LambdaJAM-Streaming-exercises/exercise_0/cabal.sandbox.config
    Creating a new sandbox at
    /home/ivan/Haskell/LambdaJAM-Streaming-exercises/exercise_0/.cabal-sandbox
    ivan exercise_0 $cabal install --only-dependencies
    Resolving dependencies...
    All the requested packages are already installed:
    Use --reinstall if you want to reinstall anyway.
    ivan exercise_0 $cabal repl
    Resolving dependencies...
    Configuring exercise0-0.1.0.0...
    Preprocessing library for exercise0-0.1.0.0..
    GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
    Loaded GHCi configuration from /home/ivan/.ghci
    [1 of 1] Compiling LambdaJAM.Streaming.Exercise0 ( LambdaJAM/Streaming/Exercise0.hs, interpreted )
    Ok, one module loaded.
    *LambdaJAM.Streaming.Exercise0>
    ```

    (You may need to run `cabal update` first.)

3. `cabal-install` + the Nix package manager: similar to above but
   with no need for a sandbox:


    ```bash
    ivan LambdaJAM-Streaming-exercises $cd exercise_0
    ivan exercise_0 $nix-shell --command "cabal configure"
    Resolving dependencies...
    Configuring exercise0-0.1.0.0...
    ivan exercise_0 $cabal repl
    Preprocessing library for exercise0-0.1.0.0..
    GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
    Loaded GHCi configuration from /home/ivan/.ghci
    [1 of 1] Compiling LambdaJAM.Streaming.Exercise0 ( LambdaJAM/Streaming/Exercise0.hs, interpreted )
    Ok, one module loaded.
    *LambdaJAM.Streaming.Exercise0>
    ```
