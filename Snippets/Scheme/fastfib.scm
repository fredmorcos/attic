(define (fastfib-helper b d n)
  (cond ((= n 1) (list 1 0))
	(else
	 (cond ((odd? n)
		(let* ((x (fastfib-helper b d (- n 1)))
		       (tmpb (+ (car x) (car (cdr x))))
		       (tmpd (car x)))
		  (list tmpb tmpd)))
	       (else
		(let* ((x (fastfib-helper b d (/ n 2)))
		       (tmpb (car x))
		       (tmpd (car (cdr x)))
		       (newb (+ (* tmpb tmpb) (* 2 (* tmpb tmpd))))
		       (newd (+ (* tmpb tmpb) (* tmpd tmpd))))
		  (list newb newd)))))))

(define (fastfib n)
  (car (fastfib-helper 1 0 n)))

(define (power b n)
  (cond ((= n 0) 1)
	((odd? n) (* b (power b (- n 1))))
	(else (power (* b b) (/ n 2)))))