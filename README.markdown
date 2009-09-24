# README: Dean Wampler's Solutions to the SICP Exercises #

Dean Wampler (dean@polyglotprogramming.com)

These are solutions for many of the SICP exercises, written in Scheme, Scala, and Clojure and driven by a unit testing library for each language. I didn't solve all the exercises. Also, I didn't solve some of the problems in all three languages (especially those that were proofs, extensions of previous exercises, etc.)

Feedback is welcome.

## Scheme Solutions ##

Most of the Scheme solutions use just the features learned up to that point. The only exceptions are a few exercises where I used output functions and the `(set! ...)` form before the book introduced them, in order to test the results.

I used *SchemeUnit* as the XUnit tool and ran the solutions using *PLT Scheme*. 

## Scala Solutions ##

I'm a self-proclaimed Scala expert: [Programming Scala](http://oreilly.com/catalog/9780596155957/) ;) Hence, some of my solutions used idioms that hadn't yet been introduced in the book. 

I used Scala v2.7.5 and [ScalaTest](http://www.artima.com/scalatest/) v0.9.5 to run the solutions. You'll need to add the `scalatest-x.y.z.jar` to the `CLASSPATH`. I invoked `scala` interactively, then loaded and executed files, *e.g.,*

    scala -cp $SCALA_TEST_HOME/scalatest-0.9.5/scalatest-0.9.5.jar
    scala> :load ex1.3.scala
    Loading ex1.3.scala...
    f: (Int,Int,Int)Int
    import org.scalatest._
    import org.scalatest.matchers._
    defined module FSpec
    Squaring top two of three integers
    - should select the top two values, square them, and add the result

(I suppressed some of the output.)

## Clojure Solutions ##

I'm a novice Clojure user, so some of the solutions may be naive!

I used the `clojure.contrib.test-is` package to write the exercises as unit tests. To run them, I invoked the REPL with the `clojure-contrib.jar` in the `CLASSPATH`, loaded each exercise solution into the REPL, then ran it using the following command:

    (run-tests)
    
Note that `(run-tests)` runs *all* the tests that have been defined in the current REPL session, not just test(s) for the last-loaded solution.
