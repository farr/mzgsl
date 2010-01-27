#lang scheme

#|  gsl-multiroot.ss: Many-dimensional root finding.
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

(require scheme/foreign
         "gsl-lib.ss"
         "define-pointer-type.ss"
         "define-gsl.ss"
         (rename-in scheme (-> ->/c)))

(unsafe!)

(define-pointer-type _gsl-multiroot-fsolver-pointer gsl-multiroot-fsolver?)
(define-pointer-type _gsl-multiroot-fsolver-type-pointer
  gsl-multiroot-fsolver-type?)
(define-pointer-type _gsl-multiroot-fdfsolver-pointer gsl-multiroot-fdfsolver?)
(define-pointer-type _gsl-multiroot-fdfsolver-type-pointer 
  gsl-multiroot-fdfsolver-type?)

(provide/contract
 (_gsl-multiroot-fsolver-type-pointer ctype?)
 (_gsl-multiroot-fsolver-pointer ctype?)
 (_gsl-multiroot-fdfsolver-type-pointer ctype?)
 (_gsl-multiroot-fdfsolver-pointer ctype?)
 (gsl-multiroot-fsolver-type? (->/c any/c boolean?))
 (gsl-multiroot-fsolver? (->/c any/c boolean?))
 (gsl-multiroot-fdfsolver-type? (->/c any/c boolean?))
 (gsl-multiroot-fdfsolver? (->/c any/c boolean?)))

(define-gsl gsl-multiroot-fsolver-alloc
  (_fun _gsl-multiroot-fsolver-type-pointer _ufixnum -> 
        (res : _gsl-multiroot-fsolver-pointer) -> 
        (begin 
          (register-finalizer res gsl-multiroot-fsolver-free)
          res)))
(define-gsl gsl-multiroot-fdfsolver-alloc
  (_fun _gsl-multiroot-fdfsolver-type-pointer _ufixnum -> 
        (res : _gsl-multiroot-fdfsolver-pointer) -> 
        (begin
          (register-finalizer res gsl-multiroot-fdfsolver-free)
          res)))

(define gsl-multiroot-fsolver-free
  (get-ffi-obj 'gsl_multiroot_fsolver_free libgsl
               (_fun _gsl-multiroot-fsolver-pointer -> _void)))
(define gsl-multiroot-fdfsolver-free
  (get-ffi-obj 'gsl_multiroot_fdfsolver_free libgsl
               (_fun _gsl-multiroot-fdfsolver-pointer -> _void)))

#| 
Stopped because first we have to wrap gsl_vector.h. 
|#