(define (f x count)
       (cond ((false? count) (f (- x 1) #t))
	     ((= x 0) 0)
	     ((or (= (modulo x 3) 0) (= (modulo x 5) 0)) (+ x (f (- x 1) #t)))
	     (else (f (- x 1) #t))))
