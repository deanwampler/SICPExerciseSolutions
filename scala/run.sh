#!/usr/bin/env sh
# Simple script to run a single exercise
# Set this variable in your shell or use the same value you used in the Makefile.
# SCALA_TEST_HOME =

scala -cp $SCALA_TEST_HOME/scalatest-1.0.jar -unchecked -deprecation "$@"
