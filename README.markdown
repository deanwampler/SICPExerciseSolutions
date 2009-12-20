# README: Dean Wampler's Solutions to the SICP Exercises #

Dean Wampler (dean@polyglotprogramming.com)

These are solutions for many of the SICP exercises, written in Scheme, Scala, and Clojure and driven by a unit testing library for each language. I didn't solve all the exercises. Also, I didn't solve some of the problems in all three languages (especially those that were proofs, extensions of previous exercises, etc.)

Feedback is welcome.

## Running the Solutions ##

There is a simple Makefile that runs all the solutions. However, it appears that failed tests don't return `$? = 1` for all the runtimes (?), so if a particular exercise solution fails, `make` won't exit.

For running individual script files, use the `run.sh` files in each language directory, *e.g.*, if your current directory is `scheme/ch02` and you want to run the Exercise 2.1 solution, then use `../run.sh ex2.1.scm`.

## Scheme Solutions ##

Most of the Scheme solutions use just the features learned up to that point. The only exceptions are a few exercises where I use output functions and the `(set! ...)` form before the book introduced them, in order to test the exercise results.

I use *SchemeUnit* as the XUnit tool and run the solutions using *PLT Scheme*, in particular the *mzscheme* interpreter. One thing I noticed about this version of scheme; there is no `nil` defined for empty lists, as used in the book.. I used `(list)` instead. 

## Scala Solutions ##

I'm a self-proclaimed Scala expert: [Programming Scala](http://oreilly.com/catalog/9780596155957/) ;) Hence, some of my solutions use idioms that haven't been introduced at that point in the book. 

I use Scala v2.7.X and [ScalaTest](http://www.artima.com/scalatest/) v1.0 to run the solutions. You'll need to add the `scalatest-x.y.jar` to the `CLASSPATH`. I invoke `scala` interactively, then load and execute files, *e.g.,*

    scala -cp $SCALA_TEST_HOME/scalatest-1.0/scalatest-1.0.jar
    scala> :load ex1.3.scala
    Loading ex1.3.scala...
    f: (Int,Int,Int)Int
    import org.scalatest._
    import org.scalatest.matchers._
    defined module FSpec
    Squaring top two of three integers
    - should select the top two values, square them, and add the result

(I suppressed some of the output.)

Sometimes I define versions of functions for which JDK implementations already exist, *e.g.*, `abs` (absolute).

## Clojure Solutions ##

I'm a novice Clojure user, so some of the solutions may be naive!

I switched to the Clojure 1.1 release candidates midstream without difficulty. The exercises probably work fine with 1.0, too. I use the `clojure.test` package that comes with the distribution to write the exercises as unit tests. To run them, I invoke the REPL, load each exercise solution into the REPL, then run it using the following command:

    (run-tests)
    
Note that `(run-tests)` runs *all* the tests that have been defined in the current REPL session, not just test(s) for the last-loaded solution.

As for my Scala solutions, sometimes I define versions of functions for which JDK or Clojure core implementations already exist, e.g., `abs` (absolute) and `even?`.

