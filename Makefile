# Makefile for Dean Wampler's SICP Exercise Solutions
# Requires bash-compatible shell.

# Set the following variables appropriately. If they are already defined in 
# your shell environment, leave the definitions commented out.
# CLOJURE_HOME =
# CLOJURE_CONTRIB_HOME =
# SCALA_TEST_HOME =

SCHEME = mzscheme
SCALA = scala
JAVA = java

clojure = $(CLOJURE_HOME)/clojure.jar
clojure-contrib = $(CLOJURE_CONTRIB_HOME)/clojure-contrib.jar
scalatest = $(SCALA_TEST_HOME)/scalatest-0.9.5.jar

ALL := all-scheme all-scala all-clojure
scheme-cmd  = $(SCHEME)
scala-cmd   = $(SCALA) -deprecation -cp $(scalatest)
clojure-cmd = $(JAVA) -cp $(clojure):$(clojure-contrib) clojure.main

all: all-scheme all-scala all-clojure

all-scheme all-scala all-clojure: 
	@echo "Building: " $(wildcard $(@:all-%=%)/ch*)
	@cmd="$($(@:all-%=%)-cmd)" ; \
	for d in $(wildcard $(@:all-%=%)/ch*); \
		do for f in $$d/*.*; \
			do echo $$cmd $$f; \
			$$cmd $$f || exit 1; \
		done; \
	done
