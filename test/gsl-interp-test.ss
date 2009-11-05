#lang scheme

(require "../gsl-interp.ss"
         (planet schematics/schemeunit:3)
         srfi/4
         "checks.ss")

(provide tests)

(define tests
  (test-suite 
   "gsl-interp.ss test suite"
   (test-case
    "cubic spline example from GSL"
    (let ((xs (make-f64vector 10))
          (ys (make-f64vector 10)))
      (for ((i (in-range 10)))
        (f64vector-set! xs i (+ i (* 1/2 (sin i))))
        (f64vector-set! ys i (+ i (cos (* i i)))))
      (let ((accel (gsl-interp-accel-alloc))
            (spline (gsl-spline-alloc gsl-interp-cspline 10)))
        (gsl-spline-init! spline xs ys)
        (let ((values (for/list ((x (in-range (f64vector-ref xs 0) (f64vector-ref xs 9) 0.01)))
                        (cons x (gsl-spline-eval spline x accel)))))
          (let ((xy0 (car values)))
            (check-close? 1e-8 1e-8 (cdr xy0) 1.0))
          (let ((xy01 (list-ref values 10)))
            (check-close? 1e-3 1e-3 (cdr xy01) 1.06972))))))))