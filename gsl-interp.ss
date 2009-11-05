#lang scheme

(require "gsl-lib.ss"
         scheme/foreign
         (rename-in scheme (-> ->/c)))

(provide/contract
 (_gsl-interp-type-pointer ctype?)
 (_gsl-interp-pointer ctype?)
 (_gsl-interp-accel-pointer ctype?)
 (_gsl-spline-pointer ctype?)
 (gsl-interp-type? (->/c any/c boolean?))
 (gsl-interp? (->/c any/c boolean?))
 (gsl-interp-accel? (->/c any/c boolean?))
 (gsl-spline? (->/c any/c boolean?)))

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

(define *gsl-interp-type-pointer-tag* (gensym '_gsl-interp-type-pointer))
(define _gsl-interp-type-pointer (_cpointer *gsl-interp-type-pointer-tag*))
(define *gsl-interp-pointer-tag* (gensym '_gsl-interp-pointer))
(define _gsl-interp-pointer (_cpointer *gsl-interp-pointer-tag*))
(define *gsl-interp-accel-pointer-tag* (gensym '_gsl-interp-accel-pointer))
(define _gsl-interp-accel-pointer (_cpointer *gsl-interp-accel-pointer-tag*))
(define *gsl-spline-pointer-tag* (gensym '_gsl-spline-pointer))
(define _gsl-spline-pointer (_cpointer *gsl-spline-pointer-tag*))

(define (gsl-interp-type? obj)
  (and (cpointer? obj)
       (cpointer-has-tag? obj *gsl-interp-type-pointer-tag*)))
(define (gsl-interp? obj)
  (and (cpointer? obj)
       (cpointer-has-tag? obj *gsl-interp-type-pointer-tag*)))
(define (gsl-interp-accel? obj)
  (and (cpointer? obj)
       (cpointer-has-tag? obj *gsl-interp-accel-pointer-tag*)))
(define (gsl-spline? obj)
  (and (cpointer? obj)
       (cpointer-has-tag? obj *gsl-spline-pointer-tag*)))

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