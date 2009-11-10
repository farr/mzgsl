#lang scheme

(require (planet schematics/schemeunit:3)
         "../gsl-odeiv.ss"
         "checks.ss"
         srfi/4)

(provide tests)

(define tests
  (test-suite
   "gsl-odeiv.ss test suite"
   (test-case 
    "Van der Pol Oscillator Example"
    (let ((f (lambda (mu) 
               (lambda (t y)
                 (let ((y1 (vector-ref y 1))
                       (y0 (vector-ref y 0)))
                   (vector y1
                           (- (- y0)
                              (* mu y1 (- (sqr y0) 1.0))))))))
          (j (lambda (mu)
               (lambda (t y)
                 (let ((y0 (vector-ref y 0))
                       (y1 (vector-ref y 1)))
                   (values 
                    (vector (vector 0 1)
                            (vector (- (* -2.0 mu y0 y1) 1.0)
                                    (* (- mu) (- (sqr y0) 1.0))))
                    (make-vector 2 0.0)))))))
      (let ((step (gsl-odeiv-step-alloc gsl-odeiv-step-rk8pd 2))
            (control (gsl-odeiv-control-y-new 1e-6 0.0))
            (evolve (gsl-odeiv-evolve-alloc 2))
            (mu 10))
        (let ((f (f mu))  ;; hang on to these references so the C
              (j (j mu))) ;; wrappers aren't collected
          (let ((sys (make-gsl-odeiv-system 
                      (make-gsl-odeiv-system-function 2 f)
                      (make-gsl-odeiv-system-jacobian 2 j)
                      2
                      #f)))
            (let ((y (f64vector 1.0 0.0))
                  (t1 100.0))
              (let loop ((t 0) (h 1e-6))
                (if (>= t t1)
                    (void)
                    (let-values (((t-new h-new)
                                  (gsl-odeiv-evolve-apply
                                   evolve control step sys t t1 h y)))
                      (loop t-new h-new))))
              (check-close? 1e-3 1e-3 (f64vector-ref y 0) -1.75889)
              (check-close? 1e-3 1e-3 (f64vector-ref y 1) 8.36436e-2)))))))))