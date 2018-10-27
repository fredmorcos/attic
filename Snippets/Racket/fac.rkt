#lang typed/racket

(: fac (Integer -> Integer))
(define (fac n)
  (cond [(zero? n) 1]
        [else (n . * . (fac (n . - . 1)))]))

(fac 4)