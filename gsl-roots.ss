#lang scheme

#|  gsl-roots.ss: Roots of functions. 
    Copyright (C) 2009 Will M. Farr <wmfarr@gmail.com>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

(require "gsl-lib.ss"
         "gsl-function.ss"
         "define-pointer-type.ss"
         "define-gsl.ss"
         scheme/foreign
         (rename-in scheme (-> ->/c)))

(unsafe!)

(define-pointer-type _gsl-root-fsolver-type-pointer gsl-root-fsolver-type?)
(define-pointer-type _gsl-root-fsolver-pointer gsl-root-fsolver?)
(define-pointer-type _gsl-root-fdfsolver-type-pointer gsl-root-fdfsolver-type?)
(define-pointer-type _gsl-root-fdfsolver-pointer gsl-root-fdfsolver?)

(provide/contract
 (_gsl-root-fsolver-type-pointer ctype?)
 (_gsl-root-fsolver-pointer ctype?)
 (_gsl-root-fdfsolver-type-pointer ctype?)
 (_gsl-root-fdfsolver-pointer ctype?)
 (gsl-root-fsolver-type? (->/c any/c boolean?))
 (gsl-root-fsolver? (->/c any/c boolean?))
 (gsl-root-fdfsolver-type? (->/c any/c boolean?))
 (gsl-root-fdfsolver? (->/c any/c boolean?)))

(define gsl-root-fsolver-free
  (get-ffi-obj 'gsl_root_fsolver_free libgsl
               (_fun _gsl-root-fsolver-pointer -> _void)))
(define gsl-root-fdfsolver-free
  (get-ffi-obj 'gsl_root_fdfsolver_free libgsl
               (_fun _gsl-root-fdfsolver-pointer -> _void)))

(define-gsl gsl-root-fsolver-alloc
  (_fun _gsl-root-fsolver-type-pointer ->
        (res : _gsl-root-fsolver-pointer) ->
        (begin
          (register-finalizer res gsl-root-fsolver-free)
          res)))
(define-gsl gsl-root-fdfsolver-alloc
  (_fun _gsl-root-fdfsolver-type-pointer ->
        (res : _gsl-root-fdfsolver-pointer) ->
        (begin
          (register-finalizer res gsl-root-fdfsolver-free)
          res)))

(define-gsl gsl-root-fsolver-bisection _gsl-root-fsolver-type-pointer)
(define-gsl gsl-root-fsolver-brent _gsl-root-fsolver-type-pointer)
(define-gsl gsl-root-fsolver-falsepos _gsl-root-fsolver-type-pointer)
(define-gsl gsl-root-fdfsolver-newton _gsl-root-fdfsolver-type-pointer)
(define-gsl gsl-root-fdfsolver-secant _gsl-root-fdfsolver-type-pointer)
(define-gsl gsl-root-fdfsolver-steffenson _gsl-root-fdfsolver-type-pointer)

(define-gsl gsl-root-fsolver-set!
  (_fun _gsl-root-fsolver-pointer _gsl-function-pointer _double* _double* -> _int))
(define-gsl gsl-root-fsolver-iterate!
  (_fun _gsl-root-fsolver-pointer -> _int))
(define-gsl gsl-root-fsolver-name
  (_fun _gsl-root-fsolver-pointer -> _string))
(define-gsl gsl-root-fsolver-x-lower
  (_fun _gsl-root-fsolver-pointer -> _double))
(define-gsl gsl-root-fsolver-x-upper
  (_fun _gsl-root-fsolver-pointer -> _double))

(define-gsl gsl-root-fdfsolver-set!
  (_fun _gsl-root-fdfsolver-pointer _gsl-function-fdf-pointer _double* -> _int))
(define-gsl gsl-root-fdfsolver-iterate!
  (_fun _gsl-root-fdfsolver-pointer -> _int))
(define-gsl gsl-root-fdfsolver-root
  (_fun _gsl-root-fdfsolver-pointer -> _double))

(define-gsl gsl-root-test-interval
  (_fun _double* _double* _double* _double* -> _bool))
(define-gsl gsl-root-test-residual
  (_fun _double* _double* -> _bool))
(define-gsl gsl-root-test-delta
  (_fun _double* _double* _double* _double* -> (x : _int) -> (= x 0)))
