#lang scheme

(require (planet schematics/schemeunit:3))

(provide check-close?)

(define-simple-check (check-close? epsabs epsrel x y)
  (let ((delta (abs (- x y)))
        (avg (* 1/2 (+ (abs x) (abs y)))))
    (<= delta (+ epsabs (* epsrel avg)))))