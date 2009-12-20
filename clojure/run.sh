#!/usr/bin/env sh
# Simple script to run a single exercise
# Set these variables in your shell or use the same values you used in the Makefile.
# CLOJURE_HOME =
# CLOJURE_CONTRIB_HOME =

#java -cp $CLOJURE_HOME/clojure.jar:$CLOJURE_CONTRIB_HOME/clojure-contrib.jar clojure.main "$@"
java -cp $CLOJURE_HOME/clojure.jar clojure.main "$@"
