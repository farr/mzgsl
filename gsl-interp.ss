#lang scheme

#|  gsl-interp.ss: Interpolation.
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
         "define-pointer-type.ss"
         scheme/foreign
         (rename-in scheme (-> ->/c)))

(unsafe!)

(define-syntax define-interp
  (syntax-rules ()
    ((define-interp name type)
     (begin 
       (provide name)
       (define name 
         (get-ffi-obj (regexp-replaces 'name '((#rx"-" "_") (#rx"!" "")))
                      libgsl
                      type))))))

(define-pointer-type _gsl-interp-type-pointer gsl-interp-type?)
(define-pointer-type _gsl-interp-pointer gsl-interp?)
(define-pointer-type _gsl-interp-accel-pointer gsl-interp-accel?)
(define-pointer-type _gsl-spline-pointer gsl-spline?)

(provide _gsl-interp-type-pointer _gsl-interp-pointer 
         _gsl-interp-accel-pointer _gsl-spline-pointer)
(provide/contract
 (gsl-interp-type? (->/c any/c boolean?))
 (gsl-interp? (->/c any/c boolean?))
 (gsl-interp-accel? (->/c any/c boolean?))
 (gsl-spline? (->/c any/c boolean?)))


(define-interp gsl-interp-alloc (_fun _gsl-interp-type-pointer _ufixnum -> 
                                      (res : _gsl-interp-pointer) -> 
                                      (begin 
                                        (register-finalizer res gsl-interp-free)
                                        res)))
(define-interp gsl-interp-init! (_fun _gsl-interp-pointer 
                                      (xs : _f64vector) _f64vector
                                      (_ufixnum = (f64vector-length xs))
                                      -> _fixnum))

(define gsl-interp-free  ;; Not exported.
  (get-ffi-obj 'gsl_interp_free libgsl 
               (_fun _gsl-interp-pointer -> _void)))

(define-interp gsl-interp-linear _gsl-interp-type-pointer)
(define-interp gsl-interp-polynomial _gsl-interp-type-pointer)
(define-interp gsl-interp-cspline _gsl-interp-type-pointer)
(define-interp gsl-interp-cspline-periodic _gsl-interp-type-pointer)
(define-interp gsl-interp-akima _gsl-interp-type-pointer)
(define-interp gsl-interp-akima-periodic _gsl-interp-type-pointer)

(define-interp gsl-interp-name 
  (_fun _gsl-interp-pointer -> _string))
(define-interp gsl-interp-min-size
  (_fun _gsl-interp-pointer -> _ufixnum))

(define-interp gsl-interp-bsearch
  (_fun _f64vector _double* _ufixnum _ufixnum -> _ufixnum))
(define-interp gsl-interp-accel-alloc
  (_fun -> (res : _gsl-interp-accel-pointer) -> 
        (begin 
          (register-finalizer res gsl-interp-accel-free)
          res)))
(define-interp gsl-interp-accel-find
  (_fun _gsl-interp-accel-pointer (xs : _f64vector) 
        (_ufixnum = (f64vector-length xs)) _double* -> _ufixnum))

(define gsl-interp-accel-free
  (get-ffi-obj 'gsl_interp_accel_free libgsl
               (_fun _gsl-interp-accel-pointer -> _void)))

(define-interp gsl-interp-eval
  (_fun _gsl-interp-pointer _f64vector _f64vector _double* 
        _gsl-interp-accel-pointer -> _double))

(define-interp gsl-interp-eval-deriv
  (_fun _gsl-interp-pointer _f64vector _f64vector _double*
        _gsl-interp-accel-pointer -> _double))

(define-interp gsl-interp-eval-deriv2
  (_fun _gsl-interp-pointer _f64vector _f64vector _double*
        _gsl-interp-accel-pointer -> _double))

(define-interp gsl-interp-eval-integ
  (_fun _gsl-interp-pointer _f64vector _f64vector _double*
        _gsl-interp-accel-pointer -> _double))

(define-interp gsl-spline-alloc
  (_fun _gsl-interp-type-pointer _ufixnum -> (res : _gsl-spline-pointer) -> 
        (begin 
          (register-finalizer res gsl-spline-free)
          res)))
(define-interp gsl-spline-init! 
  (_fun _gsl-spline-pointer (xs : _f64vector) _f64vector 
        (_ufixnum = (f64vector-length xs)) -> _fixnum))

(define gsl-spline-free
  (get-ffi-obj 'gsl_spline_free libgsl
               (_fun _gsl-spline-pointer -> _void)))

(define-interp gsl-spline-name (_fun _gsl-spline-pointer -> _string))
(define-interp gsl-spline-min-size (_fun _gsl-spline-pointer -> _ufixnum))

(define-interp gsl-spline-eval
  (_fun _gsl-spline-pointer _double* _gsl-interp-accel-pointer -> _double))
(define-interp gsl-spline-eval-deriv
  (_fun _gsl-spline-pointer _double* _gsl-interp-accel-pointer -> _double))
(define-interp gsl-spline-eval-deriv2 
  (_fun _gsl-spline-pointer _double* _gsl-interp-accel-pointer -> _double))
(define-interp gsl-spline-eval-integ
  (_fun _gsl-spline-pointer _double* _gsl-interp-accel-pointer -> _double))