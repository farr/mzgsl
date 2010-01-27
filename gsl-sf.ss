#lang scheme

#|
    Special functions from the GSL.
    Copyright (C) 2010 Will M. Farr <wmfarr@gmail.com>

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
         scheme/foreign)

(unsafe!)

(define-cstruct _gsl-sf-result
  ((val _double*)
   (err _double*)))

(define-for-syntax (ensure-symbol obj)
  (cond
   ((string? obj) (string->symbol obj))
   ((symbol? obj) obj)
   (else (error 'ensure-symbol "expected string or symbol, got " obj))))

(define-for-syntax (ensure-string obj)
  (cond
   ((string? obj) obj)
   ((symbol? obj) (symbol->string obj))
   (else (error 'ensure-string "expected string or symbol, got " obj))))

(define-for-syntax append-to-symbol
  (case-lambda
    ((x) (ensure-symbol x))
    ((x y) (string->symbol (string-append (ensure-string x) (ensure-string y))))
    ((x y . zs) (apply append-to-symbol (append-to-symbol x y) zs))))

(define-syntax define-sf-error
  (lambda (stx)
    (syntax-case stx ()
      ((define-sf-error name)
       (with-syntax ((name/error (datum->syntax (syntax name)
                                                (append-to-symbol (syntax->datum (syntax name)) "/error")
                                                (syntax name)))
                     (name_e (datum->syntax (syntax name)
                                            (symbol->string (append-to-symbol (syntax->datum (syntax name)) "_e"))
                                            (syntax name))))
         (syntax/loc stx
           (begin
             (define name
               (get-ffi-obj (regexp-replaces 'name '((#rx"-" "_"))) libgsl (_fun _double* -> _double)))
             (define name/error
               (get-ffi-obj (regexp-replaces name_e '((#rx"-" "_"))) libgsl
                                             (_fun _double*
                                                   (res : _gsl-sf-result-pointer = (make-gsl-sf-result 0.0 0.0))
                                                   -> _fixnum ->
                                                   (values (gsl-sf-result-val res)
                                                           (gsl-sf-result-err res))))))))))))

(define-sf-error gsl-sf-bessel-J0)
(define-sf-error gsl-sf-bessel-J1)
(define-sf-error gsl-sf-bessel-Y0)
(define-sf-error gsl-sf-bessel-Y1)
(define-sf-error gsl-sf-bessel-I0)
(define-sf-error gsl-sf-bessel-I1)
(define-sf-error gsl-sf-bessel-I0-scaled)
(define-sf-error gsl-sf-bessel-I1-scaled)
(define-sf-error gsl-sf-bessel-K0)
(define-sf-error gsl-sf-bessel-K1)
(define-sf-error gsl-sf-bessel-K0-scaled)
(define-sf-error gsl-sf-bessel-K1-scaled)
(define-sf-error gsl-sf-bessel-j0)
(define-sf-error gsl-sf-bessel-j1)
(define-sf-error gsl-sf-bessel-j2)
(define-sf-error gsl-sf-bessel-y0)
(define-sf-error gsl-sf-bessel-y1)
(define-sf-error gsl-sf-bessel-y2)
(define-sf-error gsl-sf-bessel-i0-scaled)
(define-sf-error gsl-sf-bessel-i1-scaled)
(define-sf-error gsl-sf-bessel-i2-scaled)
(define-sf-error gsl-sf-bessel-k0-scaled)
(define-sf-error gsl-sf-bessel-k1-scaled)
(define-sf-error gsl-sf-bessel-k2-scaled)

(define-sf-error gsl-sf-clausen)

(define-sf-error gsl-sf-dawson)

(define-sf-error gsl-sf-debye-1)
(define-sf-error gsl-sf-debye-2)
(define-sf-error gsl-sf-debye-3)
(define-sf-error gsl-sf-debye-4)
(define-sf-error gsl-sf-debye-5)
(define-sf-error gsl-sf-debye-6)

(define-sf-error gsl-sf-dilog)

(define-sf-error gsl-sf-erf)
(define-sf-error gsl-sf-erfc)
(define-sf-error gsl-sf-log-erfc)
(define-sf-error gsl-sf-erf-Z)
(define-sf-error gsl-sf-erf-Q)
(define-sf-error gsl-sf-hazard)

(define-sf-error gsl-sf-exp)
(define-sf-error gsl-sf-expm1)
(define-sf-error gsl-sf-exprel)
(define-sf-error gsl-sf-exprel-2)

(define-sf-error gsl-sf-expint-E1)
(define-sf-error gsl-sf-expint-E2)
(define-sf-error gsl-sf-expint-Ei)
(define-sf-error gsl-sf-Shi)
(define-sf-error gsl-sf-Chi)
(define-sf-error gsl-sf-expint-3)
(define-sf-error gsl-sf-Si)
(define-sf-error gsl-sf-Ci)
(define-sf-error gsl-sf-atanint)

(define-sf-error gsl-sf-fermi-dirac-m1)
(define-sf-error gsl-sf-fermi-dirac-0)
(define-sf-error gsl-sf-fermi-dirac-1)
(define-sf-error gsl-sf-fermi-dirac-2)
(define-sf-error gsl-sf-fermi-dirac-mhalf)
(define-sf-error gsl-sf-fermi-dirac-half)
(define-sf-error gsl-sf-fermi-dirac-3half)

(define-sf-error gsl-sf-gamma)
(define-sf-error gsl-sf-lngamma)
(define-sf-error gsl-sf-gammastar)
(define-sf-error gsl-sf-gammainv)

(define-sf-error gsl-sf-lambert-W0)
(define-sf-error gsl-sf-lambert-Wm1)

(define-sf-error gsl-sf-legendre-P1)
(define-sf-error gsl-sf-legendre-P2)
(define-sf-error gsl-sf-legendre-P3)
(define-sf-error gsl-sf-legendre-Q0)
(define-sf-error gsl-sf-legendre-Q1)

(define-sf-error gsl-sf-log)
(define-sf-error gsl-sf-log-abs)
(define-sf-error gsl-sf-log-1plusx)
(define-sf-error gsl-sf-log-1plusx-mx)

(define-sf-error gsl-sf-psi)
(define-sf-error gsl-sf-psi-1piy)
(define-sf-error gsl-sf-psi-1)

(define-sf-error gsl-sf-synchrotron-1)
(define-sf-error gsl-sf-synchrotron-2)

(define-sf-error gsl-sf-transport-2)
(define-sf-error gsl-sf-transport-3)
(define-sf-error gsl-sf-transport-4)
(define-sf-error gsl-sf-transport-5)

(define-sf-error gsl-sf-sin)
(define-sf-error gsl-sf-cos)
(define-sf-error gsl-sf-sinc)
(define-sf-error gsl-sf-lnsinh)
(define-sf-error gsl-sf-lncosh)

(define-sf-error gsl-sf-zeta)
(define-sf-error gsl-sf-zetam1)
(define-sf-error gsl-sf-eta)
