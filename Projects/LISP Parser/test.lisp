((() '()))
((
  '(def foo "hello")                    ; this is a comment
  :hello :world :there! "string" :symbol?)) ; this is another one!

((()()))
((
  ()
  ))

;; Recursive
(define (factorial n)
  (if (<= n 0)
      1
      (* n (factorial (- n 1)))))

;; Tail recursive
(define (factorial-tail n x)
  (if (<= n 0)
      x
      (factorial-tail (- n 1) (* x n))))

(print (factorial 100))
(print (factorial-tail 100 1))

;; Recursive
(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

;; Tail recursive
(define (fibonacci-tail n a b)
  (if (= n 0)
      a
      (fibonacci-tail (- n 1) (+ b a) a)))

(print (fibonacci 40))
(print (fibonacci-tail 40 0 1))
