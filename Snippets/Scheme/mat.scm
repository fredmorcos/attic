(define mat1 (list (list 0  1  2  3)
		   (list 4  5  6  7)
		   (list 8  9  10 11)
		   (list 12 13 14 15)))

(define mat2 (list (list 0  1  2  3)
		   (list 4  5  6  7)
		   (list 8  9  10 11)
		   (list 12 13 14 15)))

(define mat3 (list (list 0  0  0  0)
		   (list 0  0  0  0)
		   (list 0  0  0  0)
		   (list 0  0  0  0)))

(define (list-set! l i v)
  (if (= i 0)
      (set-car! l v)
      (list-set! (cdr l) (- i 1) v)))

(define (mat-get-val l i j)
  (list-ref (list-ref l i) j))

(define (mat-set-val l i j v)
  (if (= i 0)
      (list-set! (car l) j v)
      (mat-set-val (cdr l) (- i 1) j v)))

(define (mat-trans m i0 i1 j0 j1)
  (let ((di (- i1 i0))
	(dj (- j1 j0)))
    (cond ((and (> di 1) (> di dj))
	   (let ((im (/ (+ i0 i1) 2)))
	     (begin
	       (mat-trans m i0 im j0 j1)
	       (mat-trans m im i1 j0 j1))))
	  ((> dj 1)
	   (let ((jm (/ (+ j0 j1) 2)))
	     (begin
	       (mat-trans m i0 i1 j0 jm)
	       (mat-trans m i0 i1 jm j1))))
	  ((< i0 j0)
	   (let ((tmp (mat-get-val m i0 j0)))
	     (begin
	       (mat-set-val m i0 j0 (mat-get-val m j0 i0))
	       (mat-set-val m j0 i0 tmp)))))))

(define (mat-mul m1 m2 m3 i0 i1 j0 j1 k0 k1)
  (let ((di (- i1 i0))
	(dj (- j1 j0))
	(dk (- k1 k0)))
    (cond ((and (> di 1) (> di dj) (> di dk))
	   (let ((im (/ (+ i0 i1) 2)))
	     (mat-mul m1 m2 m3 i0 im j0 j1 k0 k1)
	     (mat-mul m1 m2 m3 im i1 j0 j1 k0 k1)))
	  ((and (> dj 1) (> dj dk))
	   (let ((jm (/ (+ j0 j1) 2)))
	     (mat-mul m1 m2 m3 i0 i1 j0 jm k0 k1)
	     (mat-mul m1 m2 m3 i0 i1 jm j1 k0 k1)))
	  ((> dk 1)
	   (let ((km (/ (+ k0 k1) 2)))
	     (mat-mul m1 m2 m3 i0 i1 j0 j1 k0 km)
	     (mat-mul m1 m2 m3 i0 i1 j0 j1 km k1)))
	  (else
	   (let* ((val1 (mat-get-val m1 i0 k0))
		  (val2 (mat-get-val m2 k0 j0))
		  (val3 (mat-get-val m3 i0 j0))
		  (val4 (+ val3 (* val1 val2))))
	     (begin
	       (mat-set-val m3 i0 j0 val4)))))))
