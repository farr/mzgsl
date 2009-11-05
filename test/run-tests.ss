#lang scheme

(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         (prefix-in gsl-roots: "gsl-roots-test.ss")
         (prefix-in gsl-interp: "gsl-interp-test.ss"))

(define tests
  (test-suite
   "all mzgsl tests"
   gsl-roots:tests
   gsl-interp:tests))

(run-tests tests)