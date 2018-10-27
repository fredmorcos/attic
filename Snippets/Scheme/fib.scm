(define (fib n)
  (cond ((= n 0) 1)
	((= n 1) 1)
	(else (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib2 n)
  (if (< n 2)
      1
      (+ (fib2 (- n 1)) (fib2 (- n 2)))))