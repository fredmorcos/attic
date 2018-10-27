(define (make-initialized-vector n f)
  (do ((v (make-vector n))
       (i 0 (+ i 1)))
      ((= i n) v)
    (vector-set! v i i)))

(define (net n)
  (cons (make-initialized-vector n (lambda (i) i))
        (make-vector n 1)))

(define (net-find-root v x)
  (let ((xval (vector-ref v x)))
    (if (= xval x) x
        (begin (vector-set! v x (vector-ref v xval))
               (net-find-root v xval)))))

(define (netw-find-root v x)
  (net-find-root (car v) x))

(define (net-connected v p q)
  (= (net-find-root v p) (net-find-root v q)))

(define (netw-connected v p q)
  (net-connected (car v) p q))

(define (net-union! v p q)
  (vector-set! v (net-find-root v p) (net-find-root v q)))

(define (netw-union! v p q)
  (let* ((n (car v))
         (w (cdr v))
         (proot (net-find-root n p))
         (qroot (net-find-root n q))
         (pwei  (vector-ref w proot))
         (qwei  (vector-ref w qroot)))
    (cond ((= proot qroot) v)
          ((>= pwei qwei) (begin (vector-set! n qroot proot)
                                 (vector-set! w proot (+ qwei pwei))
                                 v))
          (else (begin (vector-set! n proot qroot)
                       (vector-set! w qroot (+ pwei qwei))
                       v)))))
