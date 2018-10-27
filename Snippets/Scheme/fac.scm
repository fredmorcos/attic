(define (fac n)
  (cond ((= n 1) 1)
	(else (* n (fac (- n 1))))
  )
)

(define (fac2 n)
  (if (< n 2) 1
      (* n (fac2 (- n 1)))))