(define (make-initialized-vector n f)
  (do ((v (make-vector n))
       (i 0 (+ i 1)))
      ((= i n) v)
    (vector-set! v i i)))

(define (net n)
  (make-initialized-vector n (lambda (i) i)))

(define (net-connected v p q)
  (= (vector-ref v p) (vector-ref v q)))

(define (union v p q)
  (let ((pval (vector-ref v p))
        (qval (vector-ref v q))
        (vlen (vector-length v)))
    (do ((i 0 (+ i 1)))
        ((= i vlen) v)
      (if (= (vector-ref v i) pval)
          (vector-set! v i qval)
          #f))))

(define (net-find-root v x)
  (let ((xval (vector-ref v x)))
    (if (= xval x) x
        (net-find-root v xval))))

(define (net-fast-connected v p q)
  (= (net-find-root v p) (net-find-root v q)))

(define (net-fast-union v p q)
  (vector-set! v (net-find-root v p) (net-find-root v q)))

(define mynet (net 10))

(net-fast-union mynet 0 1)
(net-fast-union mynet 4 9)
(net-fast-union mynet 1 4)

(print (net-fast-connected mynet 0 9))
