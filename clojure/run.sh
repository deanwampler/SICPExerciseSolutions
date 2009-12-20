#!/usr/bin/env sh
# Simple script to run a single exercise
# Set these variables in your shell or use the same values you used in the Makefile.
# CLOJURE_HOME =

java -cp $CLOJURE_HOME/clojure.jar clojure.main "$@"
