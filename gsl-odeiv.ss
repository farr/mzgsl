#lang scheme

#|  gsl-odeiv.ss: Initial-value ODE problems. 
    Copyright (C) 2009 Will M. Farr <wmfarr@gmail.com>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
|#

(require "gsl-lib.ss"
         "define-pointer-type.ss"
         "define-gsl.ss"
         scheme/foreign
         (rename-in scheme (-> ->/c)))

(provide _gsl-odeiv-system _gsl-odeiv-system-pointer
         make-gsl-odeiv-system gsl-odeiv-system? 
         gsl-odeiv-system-function gsl-odeiv-system-jacobian 
         gsl-odeiv-system-dimension gsl-odeiv-system-params)

(unsafe!)

(define-cstruct _gsl-odeiv-system
  ((function (_fun _double _pointer _pointer _pointer -> _fixnum))
   (jacobian (_fun _double _pointer _pointer _pointer _pointer -> _fixnum))
   (dimension _ufixnum)
   (params _pointer)))

(define (double-ptr->vector size dp)
  (let ((v (make-vector size)))
    (for ((i (in-range size)))
      (vector-set! v i (ptr-ref dp _double i)))
    v))

(define (vector->double-ptr! v dp)
  (for ((i (in-range (vector-length v))))
    (ptr-set! dp _double i (vector-ref v i))))

(define (make-gsl-odeiv-system-function size scheme-proc)
  (lambda (t y dydt params)
    (let ((dydtv (scheme-proc t (double-ptr->vector size y))))
      (vector->double-ptr! dydtv dydt)
      0)))

(define (make-gsl-odeiv-system-jacobian size scheme-proc)
  (lambda (t y dfdy dfdt params)
    (let-values (((dfdyv dfdtv)
                  (scheme-proc t (double-ptr->vector size y))))
      (vector->double-ptr! dfdtv dfdt)
      (for* ((i (in-range size))
             (j (in-range size)))
        (ptr-set! dfdy _double (+ j (* i size)) 
                  (vector-ref (vector-ref dfdyv i) j)))
      0)))

(provide/contract
 (make-gsl-odeiv-system-function 
  (->/c natural-number/c 
        (->/c real? (vectorof real?) (vectorof real?))
        (->/c real? cpointer? cpointer? any/c natural-number/c)))
 (make-gsl-odeiv-system-jacobian
  (->/c natural-number/c 
        (->/c real? (vectorof real?) (values (vectorof real?) (vectorof real?)))
        (->/c real? cpointer? cpointer? cpointer? any/c natural-number/c))))
        

(define-pointer-type _gsl-odeiv-step-type-pointer gsl-odeiv-step-type?)
(define-pointer-type _gsl-odeiv-step-pointer gsl-odeiv-step?)

(provide/contract 
 (_gsl-odeiv-step-type-pointer ctype?)
 (_gsl-odeiv-step-pointer ctype?)
 (gsl-odeiv-step-type? (->/c any/c boolean?))
 (gsl-odeiv-step? (->/c any/c boolean?)))

(define-gsl gsl-odeiv-step-alloc 
  (_fun _gsl-odeiv-step-type-pointer _ufixnum -> 
        (step : _gsl-odeiv-step-pointer) -> 
        (begin 
          (register-finalizer step gsl-odeiv-step-free)
          step)))
(define gsl-odeiv-step-free 
  (get-ffi-obj 'gsl_odeiv_step_free libgsl 
               (_fun _gsl-odeiv-step-pointer -> _void)))

(define-gsl gsl-odeiv-step-reset! 
  (_fun _gsl-odeiv-step-pointer -> _fixnum))

(define-gsl gsl-odeiv-step-name 
  (_fun _gsl-odeiv-step-pointer -> _string))
(define-gsl gsl-odeiv-step-order 
  (_fun _gsl-odeiv-step-pointer -> _ufixnum))

(define-gsl gsl-odeiv-step-apply 
  (_fun _gsl-odeiv-step-pointer _double* _double* 
        _f64vector _f64vector _f64vector _f64vector 
        _gsl-odeiv-system-pointer -> _fixnum))

(define-gsl gsl-odeiv-step-rk2 _gsl-odeiv-step-type-pointer)
(define-gsl gsl-odeiv-step-rkf45 _gsl-odeiv-step-type-pointer)
(define-gsl gsl-odeiv-step-rkck _gsl-odeiv-step-type-pointer)
(define-gsl gsl-odeiv-step-rk8pd _gsl-odeiv-step-type-pointer)
(define-gsl gsl-odeiv-step-rk2imp _gsl-odeiv-step-type-pointer)
(define-gsl gsl-odeiv-step-rk4imp _gsl-odeiv-step-type-pointer)
(define-gsl gsl-odeiv-step-bsimp _gsl-odeiv-step-type-pointer)
(define-gsl gsl-odeiv-step-gear1 _gsl-odeiv-step-type-pointer)
(define-gsl gsl-odeiv-step-gear2 _gsl-odeiv-step-type-pointer)

(define-pointer-type _gsl-odeiv-control-pointer gsl-odeiv-control?)
(provide/contract 
 (_gsl-odeiv-control-pointer ctype?)
 (gsl-odeiv-control? (->/c any/c boolean?)))

(define-gsl gsl-odeiv-control-standard-new 
  (_fun _double* _double* _double* _double* -> 
        (res : _gsl-odeiv-control-pointer) -> 
        (begin 
          (register-finalizer res gsl-odeiv-control-free)
          res)))
(define-gsl gsl-odeiv-control-y-new
  (_fun _double* _double* -> 
        (res : _gsl-odeiv-control-pointer) -> 
        (begin
          (register-finalizer res gsl-odeiv-control-free)
          res)))
(define-gsl gsl-odeiv-control-yp-new
  (_fun _double* _double* -> 
        (res : _gsl-odeiv-control-pointer) -> 
        (begin
          (register-finalizer res gsl-odeiv-control-free)
          res)))
(define-gsl gsl-odeiv-control-scaled-new
  (_fun _double* _double* _double* _double* (scale-abs : _f64vector) 
        (_ufixnum = (f64vector-length scale-abs)) -> 
        (res : _gsl-odeiv-control-pointer) -> 
        (begin
          (register-finalizer res gsl-odeiv-control-free)
          res)))

(define gsl-odeiv-control-free
  (get-ffi-obj 'gsl_odeiv_control_free libgsl
               (_fun _gsl-odeiv-control-pointer -> _void)))

(define-gsl gsl-odeiv-control-hadjust
  (_fun _gsl-odeiv-control-pointer _gsl-odeiv-step-pointer
        _f64vector _f64vector _f64vector (h : (_ptr io _double*)) -> 
        _fixnum -> 
        h))

(define-gsl gsl-odeiv-control-name 
  (_fun _gsl-odeiv-control-pointer -> _string))

(define-pointer-type _gsl-odeiv-evolve-pointer gsl-odeiv-evolve?)
(provide/contract
 (_gsl-odeiv-evolve-pointer ctype?)
 (gsl-odeiv-evolve? (->/c any/c boolean?)))

(define-gsl gsl-odeiv-evolve-alloc
  (_fun _ufixnum -> 
        (res : _gsl-odeiv-evolve-pointer) -> 
        (begin
          (register-finalizer res gsl-odeiv-evolve-free)
          res)))

(define-gsl gsl-odeiv-evolve-apply
  (_fun _gsl-odeiv-evolve-pointer _gsl-odeiv-control-pointer
        _gsl-odeiv-step-pointer _gsl-odeiv-system-pointer
        (t : (_ptr io _double*)) _double* (h : (_ptr io _double*))
        _f64vector -> _fixnum -> (values t h)))

(define-gsl gsl-odeiv-evolve-reset!
  (_fun _gsl-odeiv-evolve-pointer -> _fixnum))

(define gsl-odeiv-evolve-free
  (get-ffi-obj 'gsl_odeiv_evolve_free libgsl
               (_fun _gsl-odeiv-evolve-pointer -> _void)))