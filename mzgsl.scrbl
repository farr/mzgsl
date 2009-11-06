#lang scribble/doc

@(require scribble/manual
          (for-label scheme)
          (for-label (except-in scheme/foreign ->))
          (for-label "gsl-rng.ss")
          (for-label "gsl-lib.ss")
          (for-label "gsl-function.ss")
          (for-label "gsl-roots.ss")
          (for-label "gsl-interp.ss"))

@title{MzGSL: MzScheme Bindings to the GNU Scientific Library}

The @filepath{mzgsl.plt} PLaneT package provides bindings to the
@link["http://www.gnu.org/software/gsl/"]{GNU Scientific Library}.  It
is currently very incomplete, but growing.  The library aims to
provide only basic bindings to the GSL; a more ``scheme-like'' library
could be constructed on top of this base, but this is not provided by
mzgsl.

You can use the bindings from @filepath{mzgsl.plt} in your programs by
issuing the command @scheme[(require (planet "all.ss" ("wmfarr"
"mzgsl.plt" 3 1)))] or, in shortened notation @scheme[(require (planet
wmfarr/mzgsl:3:1/all))].  The @filepath["all.ss"] module exports all
bindings of the @filepath{mzgsl.plt} package.

The mzgsl.plt package is released under the GPL; see the
@secref["License"] section of this document for more information.
@filepath{mzgsl.plt} is maintained by
@link["mailto:wmfarr@gmail.com"]{Will M. Farr}; send feature requests and
bug reports to that address.

@section[#:tag "FFI-libraries"]{FFI Libraries}

@defmodule[(planet wmfarr/mzgsl:3:1/gsl-lib)]

The system makes a reasonable effort to find the libraries for the GSL
and GSL's CBLAS on your system; if it cannot, report it as a bug, and
I'll add the appropriate code to locate the GSL on your system.

@deftogether[((defthing libgsl any/c) (defthing libgslcblas any/c))]{

ffi-lib objects which point to the corresponding foreign library.

}

@section[#:tag "RNG"]{Random Number Generation}

@defmodule[(planet wmfarr/mzgsl:3:1/gsl-rng)]

The following functions are provided by @filepath["gsl-rng.ss"].  The
naming scheme follows very closely the GSL naming scheme, with
@scheme[_] replaced by @scheme[-], and an @scheme[!] inserted in
@scheme[gsl-rng-set!].  Note also that @scheme[gsl-rng-state-set!] is
provided even though that is not a function in the GSL library.

@subsection[#:tag "RNG:FFI-values"]{Basic FFI Values and Predicates}

@deftogether[
             ((defthing _gsl-rng-type-pointer any/c) 
              (defproc (gsl-rng-type? (obj any/c)) boolean?) 
              (defthing _gsl-rng-pointer any/c)
              (defproc (gsl-rng? (obj any/c)) boolean?)
              (defthing _gsl-rng-state-pointer any/c)
              (defproc (gsl-rng-state? (obj any/c)) boolean?))]{

These are the foreign types and corresponding predicates for objects
from the GSL RNG library.

}

@subsection[#:tag "RNG:types"]{RNG Types}

@deftogether[
             ((defthing gsl-rng-borosh13 gsl-rng-type?)
              (defthing gsl-rng-coveyou gsl-rng-type?)
              (defthing gsl-rng-cmrg gsl-rng-type?)
              (defthing gsl-rng-fishman18 gsl-rng-type?)
              (defthing gsl-rng-fishman20 gsl-rng-type?)
              (defthing gsl-rng-fishman2x gsl-rng-type?)
              (defthing gsl-rng-gfsr4 gsl-rng-type?)
              (defthing gsl-rng-knuthran gsl-rng-type?)
              (defthing gsl-rng-knuthran2 gsl-rng-type?)
              (defthing gsl-rng-knuthran2002 gsl-rng-type?)
              (defthing gsl-rng-lecuyer21 gsl-rng-type?)
              (defthing gsl-rng-minstd gsl-rng-type?)
              (defthing gsl-rng-mrg gsl-rng-type?)
              (defthing gsl-rng-mt19937 gsl-rng-type?)
              (defthing gsl-rng-mt19937-1999 gsl-rng-type?)
              (defthing gsl-rng-mt19937-1998 gsl-rng-type?)
              (defthing gsl-rng-r250 gsl-rng-type?)
              (defthing gsl-rng-ran0 gsl-rng-type?)
              (defthing gsl-rng-ran1 gsl-rng-type?)
              (defthing gsl-rng-ran2 gsl-rng-type?)
              (defthing gsl-rng-ran3 gsl-rng-type?)
              (defthing gsl-rng-rand gsl-rng-type?)
              (defthing gsl-rng-rand48 gsl-rng-type?)
              (defthing gsl-rng-random128-bsd gsl-rng-type?)
              (defthing gsl-rng-random128-glibc2 gsl-rng-type?)
              (defthing gsl-rng-random128-libc5 gsl-rng-type?)
              (defthing gsl-rng-random256-bsd gsl-rng-type?)
              (defthing gsl-rng-random256-glibc2 gsl-rng-type?)
              (defthing gsl-rng-random256-libc5 gsl-rng-type?)
              (defthing gsl-rng-random32-bsd gsl-rng-type?)
              (defthing gsl-rng-random32-glibc2 gsl-rng-type?)
              (defthing gsl-rng-random32-libc5 gsl-rng-type?)
              (defthing gsl-rng-random64-bsd gsl-rng-type?)
              (defthing gsl-rng-random64-glibc2 gsl-rng-type?)
              (defthing gsl-rng-random64-libc5 gsl-rng-type?)
              (defthing gsl-rng-random8-bsd gsl-rng-type?)
              (defthing gsl-rng-random8-glibc2 gsl-rng-type?)
              (defthing gsl-rng-random8-libc5 gsl-rng-type?)
              (defthing gsl-rng-random-bsd gsl-rng-type?)
              (defthing gsl-rng-random-glibc2 gsl-rng-type?)
              (defthing gsl-rng-random-libc5 gsl-rng-type?)
              (defthing gsl-rng-randu gsl-rng-type?)
              (defthing gsl-rng-ranf gsl-rng-type?)
              (defthing gsl-rng-ranlux gsl-rng-type?)
              (defthing gsl-rng-ranlux389 gsl-rng-type?)
              (defthing gsl-rng-ranlxd1 gsl-rng-type?)
              (defthing gsl-rng-ranlxd2 gsl-rng-type?)
              (defthing gsl-rng-ranlxs0 gsl-rng-type?)
              (defthing gsl-rng-ranlxs1 gsl-rng-type?)
              (defthing gsl-rng-ranlxs2 gsl-rng-type?)
              (defthing gsl-rng-ranmar gsl-rng-type?)
              (defthing gsl-rng-slatec gsl-rng-type?)
              (defthing gsl-rng-taus gsl-rng-type?)
              (defthing gsl-rng-taus2 gsl-rng-type?)
              (defthing gsl-rng-taus113 gsl-rng-type?)
              (defthing gsl-rng-transputer gsl-rng-type?)
              (defthing gsl-rng-tt800 gsl-rng-type?)
              (defthing gsl-rng-uni gsl-rng-type?)
              (defthing gsl-rng-uni32 gsl-rng-type?)
              (defthing gsl-rng-vax gsl-rng-type?)
              (defthing gsl-rng-waterman14 gsl-rng-type?)
              (defthing gsl-rng-zuf gsl-rng-type?))]{

The various types of generators provided by the GSL.  

}

@subsection[#:tag "RNG:Functions"]{RNG Functions}

@deftogether[
             ((defproc (gsl-rng-alloc (type gsl-rng-type?)) gsl-rng?)
              (defproc (gsl-rng-memcpy (target gsl-rng?) (source gsl-rng?)) any)
              (defproc (gsl-rng-clone (rng gsl-rng?)) gsl-rng?))]{

Allocating and modifying GSL RNGs.  Note that a @scheme[gsl-rng-free]
procedure is *not* provided by the library; memory management is
automatic in this library.  The alloc and clone functions
automatically register a finalizer for the allocated generator.

}

@deftogether[
             ((defproc (gsl-rng-set! (rng gsl-rng?) (seed integer?)) any)
              (defproc (gsl-rng-state-set! (rng gsl-rng?) (state gsl-rng-state?)) any))]{

Set the internal state of the generator. 

}

@deftogether[
             ((defproc (gsl-rng-max (rng gsl-rng?)) integer?)
              (defproc (gsl-rng-min (rng gsl-rng?)) integer?)
              (defproc (gsl-rng-name (rng gsl-rng?)) string?)
              (defproc (gsl-rng-size (rng gsl-rng?)) integer?))]{

Query the properties of a particular generator.

}

@defproc[(gsl-rng-state (rng gsl-rng?)) gsl-rng-state?]{

Get the internal state of the generator.

}

@deftogether[
             ((defproc (gsl-rng-get (rng gsl-rng?)) integer?)
              (defproc (gsl-rng-uniform (rng gsl-rng?)) real?)
              (defproc (gsl-rng-uniform-pos (rng gsl-rng?)) real?)
              (defproc (gsl-rng-uniform-int (rng gsl-rng) (max integer?)) integer?))]{

Get random numbers from the generator.

}

@section[#:tag "GSL Functions"]{GSL Functions}
@defmodule[(planet wmfarr/mzgsl:3:1/gsl-function)]

@deftogether[
(@defthing[_gsl-function ctype?]
 @defthing[_gsl-function-pointer ctype?]
 @defproc[(make-gsl-function 
           (function (_fun _double _pointer -> _double))
           (params _pointer))
          gsl-function?]
 @defproc[(gsl-function? (obj any/c)) boolean?]
 @defproc[(gsl-function-function (fun gsl-function?))
          (_fun _double _pointer -> _double)]
 @defproc[(gsl-function-params (fun gsl-function?)) cpointer?])]{

Wraps scheme functions as GSL ``closures.''  When a scheme function is
wrapped, a corresponding C function is generated on the fly that takes
the appropriate arguments, wraps them, and then calls out to the
scheme function.  This C-side wrapper will exist only as long as the
corresponding scheme function exists, so it is important to hang on to
a reference to your scheme functions that you store in
@scheme[__gsl-function] structs.  Also, each scheme function that is
wrapped in this way can only ``store'' one C-side function, so don't
wrap the same function in multiple @scheme[__gsl-function] structs.
For more on this, see the documentation of the @scheme[#:keep?]
argument to @scheme[_cprocedure]; the functions stored in a
@scheme[__gsl-function] use the @scheme[#t] argument to
@scheme[_cprocedure]'s @scheme[#:keep].

}

@deftogether[
(@defthing[_gsl-function-fdf ctype?]
 @defthing[_gsl-function-fdf-pointer ctype?]
 @defproc[(make-gsl-function-fdf 
           (f (_fun _double* _pointer -> _double*))
           (df (_fun _double _pointer -> _double*))
           (f-df (_fun _double* _pointer _pointer _pointer -> _void))
           (params _pointer))
          gsl-function-fdf?]
 @defproc[(gsl-function-fdf? (obj any/c)) boolean?]
 @defproc[(gsl-function-fdf-f (fun gsl-function-fdf?)) (_fun _double* _pointer -> _double*)]
 @defproc[(gsl-function-fdf-df 
           (fun gsl-function-fdf?))
          (_fun _double _pointer -> _double*)]
 @defproc[(gsl-function-fdf-f-df (fun gsl-function-fdf?))
          (_fun _double* _pointer _pointer _pointer -> _void)]
 @defproc[(gsl-function-fdf-params (fun gsl-function-fdf?)) cpointer?])]{

Same as @scheme[_gsl-function], but for functions with derivative.

}

@section[#:tag "gsl-roots"]{Root Finding}
@defmodule[(planet wmfarr/mzgsl:3:1/gsl-roots)]

This section discusses the wrapper for the root finding procedures in
the GSL.

@deftogether[
(@defproc[(gsl-root-fsolver-type? (obj any/c)) boolean?]
 @defproc[(gsl-root-fsolver? (obj any/c)) boolean?]
 @defproc[(gsl-root-fdfsolver-type? (obj any/c)) boolean?]
 @defproc[(gsl-root-fdfsolver? (obj any/c)) boolean?])]{

Predicates for the types in this module.                              

}

@deftogether[
(@defproc[(gsl-root-fsolver-alloc (type gsl-root-fsolver-type?)) 
          gsl-root-fsolver?]
 @defproc[(gsl-root-fdfsolver-alloc (type gsl-root-fdfsolver-type?))
         gsl-root-fdfsolver?])]{

Allocate solvers.

}

@deftogether[
(@defthing[gsl-root-fsolver-bisection gsl-root-fsolver-type?]
 @defthing[gsl-root-fsolver-brent gsl-root-fsolver-type?]
 @defthing[gsl-root-fsolver-falsepos gsl-root-fsolver-type?])]{

Root finders that do not require derivative information.

}

@deftogether[
(@defthing[gsl-root-fdfsolver-newton gsl-root-fdfsolver-type?]
 @defthing[gsl-root-fdfsolver-secant gsl-root-fdfsolver-type?]
 @defthing[gsl-root-fdfsolver-steffenson gsl-root-fdfsolver-type?])]{

Root finders that do require derivative information.

}

@deftogether[
(@defproc[(gsl-root-fsolver-set! 
           (solver gsl-root-fsolver?)
           (f gsl-function?)
           (x-min real?)
           (x-max real?))
          integer?]
 @defproc[(gsl-root-fsolver-iterate! (solver gsl-root-fsolver?)) integer?]
 @defproc[(gsl-root-fsolver-name (solver gsl-root-fsolver?)) string?]
 @defproc[(gsl-root-fsolver-x-lower (solver gsl-root-fsolver?)) real?]
 @defproc[(gsl-root-fsolver-x-upper (solver gsl-root-fsolver?)) real?])]{

Procedures to manipulate solvers that do not require derivatives. 

}

@deftogether[
(@defproc[(gsl-root-fdfsolver-set! 
           (solver gsl-root-fdfsolver?)
           (f gsl-function-fdf?)
           (x real?))
          integer?]
 @defproc[(gsl-root-fdfsolver-iterate! (solver gsl-root-fdfsolver?)) integer?]
 @defproc[(gsl-root-fdfsolver-root (solver gsl-root-fdfsolver?)) real?])]{

Procedures to manipulate solvers that do require derivatives.

}

@deftogether[
(@defproc[(gsl-root-test-interval 
           (x-lower real?)
           (x-upper real?)
           (epsabs real?)
           (epsrel real?))
          boolean?]
 @defproc[(gsl-root-test-residual (f real?) (epsabs real?)) boolean?]
 @defproc[(gsl-root-test-delta
           (x1 real?)
           (x0 real?)
           (epsabs real?)
           (epsrel real?))
          boolean?])]{

Iteration-stopping test functions.

}

@section[#:tag "Interpolation"]{Interpolation}
@defmodule[(planet wmfarr/mzgsl:3:1/gsl-interp)]

This section discusses the bindings to the @filepath["gsl_interp.h"]
and @filepath["gsl_spline.h"] functions.

@deftogether[
(@defthing[_gsl-interp-type-pointer ctype?]
 @defthing[_gsl-interp-pointer ctype?]
 @defthing[_gsl-interp-accel-pointer ctype?]
 @defthing[_gsl-spline-pointer ctype?]
 @defproc[(gsl-interp-type? (obj any/c)) boolean?]
 @defproc[(gsl-interp? (obj any/c)) boolean?]
 @defproc[(gsl-interp-accel? (obj any/c)) boolean?]
 @defproc[(gsl-spline? (obj any/c)) boolean?])]{

Foreign types and predicates for this module.

}

@deftogether[
(@defproc[(gsl-interp-alloc (type gsl-interp-type?) (size integer?))
          gsl-interp?]
 @defproc[(gsl-interp-init! (interp gsl-interp?) (xs f64vector?)
                            (ys f64vector?) (size integer?))
          integer?])]{

Allocate and initialize an interpolation object. 

}

@deftogether[
(@defthing[gsl-interp-linear gsl-interp-type?]
 @defthing[gsl-interp-polynomial gsl-interp-type?]
 @defthing[gsl-interp-cspline gsl-interp-type?]
 @defthing[gsl-interp-cspline-periodic gsl-interp-type?]
 @defthing[gsl-interp-akima gsl-interp-type?]
 @defthing[gsl-interp-akima-periodic gsl-interp-type?])]{

Interpolation types.

}

@deftogether[
(@defproc[(gsl-interp-name (interp gsl-interp?)) string?]
 @defproc[(gsl-interp-min-size (interp gsl-interp?)) integer?])]{

Queries for interp object properties.

}

@deftogether[
(@defproc[(gsl-interp-bsearch (xs f64vector?) (x real?) 
                              (low-index natural-number/c)
                              (high-index natural-number/c))
          natural-number/c]
 @defproc[(gsl-interp-accel-alloc) gsl-interp-accel?]
 @defproc[(gsl-interp-accel-find (acc gsl-interp-accel?)
                                 (xs f64vector?)
                                 (x real?))
          natural-number/c])]{

Accelerator object procedures. 

}

@deftogether[
(@defproc[(gsl-interp-eval (interp gsl-interp?) (xs f64vector?)
                           (ys f64vector?) (x real?) (acc gsl-interp-accel?))
          real?]
 @defproc[(gsl-interp-eval-deriv (interp gsl-interp?) (xs f64vector?)
                           (ys f64vector?) (x real?) (acc gsl-interp-accel?))
          real?]
 @defproc[(gsl-interp-eval-deriv2 (interp gsl-interp?) (xs f64vector?)
                           (ys f64vector?) (x real?) (acc gsl-interp-accel?))
          real?]
 @defproc[(gsl-interp-eval-integ (interp gsl-interp?) (xs f64vector?)
                           (ys f64vector?) (x real?) (acc gsl-interp-accel?))
          real?])]{

Evaluate interpolations and their derivatives and integrals. 

}

@deftogether[
(@defproc[(gsl-spline-alloc (type gsl-interp-type?) (size natural-number/c))
          gsl-spline?]
 @defproc[(gsl-spline-init! (spline gsl-spline?)
                            (xs f64vector?) (ys f64vector?))
          integer?])]{

Allocate and initialize a spline object.  Splines are more convenient
packages for fitting than interp objects because they store the x- and
y-values internally instead of getting them in a procedure call.

}

@deftogether[
(@defproc[(gsl-spline-name (sp gsl-spline?)) string?]
 @defproc[(gsl-spline-min-size (sp gsl-spline?)) natural-number/c])]{

Queries for spline object properties.

}

@deftogether[
(@defproc[(gsl-spline-eval (sp gsl-spline?) (x real?) (acc gsl-interp-accel?))
          real?]
 @defproc[(gsl-spline-eval-deriv
           (sp gsl-spline?) (x real?) (acc gsl-interp-accel?))
          real?]
 @defproc[(gsl-spline-eval-deriv2
           (sp gsl-spline?) (x real?) (acc gsl-interp-accel?))
          real?]
 @defproc[(gsl-spline-eval-integ
           (sp gsl-spline?) (x real?) (acc gsl-interp-accel?))
          real?])]{

Evaluate the interpolation stored in a spline.

}

@section[#:tag "License"]{License}

The @filepath["mzgsl.plt"] package is released under the GPL, Version
3.  You can find a copy of this license in the
@link["../../gpl-3.0.txt"]{main directory} of the package, or
@link["http://www.gnu.org/licenses/gpl.html"]{here}.
