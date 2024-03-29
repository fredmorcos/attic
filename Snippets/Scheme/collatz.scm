(define (collatz n)
  (cond
   ((= n 1) #t)
    ;; (display "finished!")
    ;; (newline))
   ((= (modulo n 2) 0)
    ;; (display "even:  ")
    ;; (display (number->string n))
    ;; (newline)
    (collatz (/ n 2)))
   (else
    ;; (display "odd:   ")
    ;; (display (number->string n))
    ;; (newline)
    (collatz (+ (* 3 n) 1)))))

(define (collatz-find n)
  ;; (display "collatz(")
  ;; (display (number->string n))
  ;; (display ")")
  ;; (newline)
  (if (= (modulo n 1000000) 0)
      (begin (display (number->string n))
             (newline)))
  (collatz n)
  ;; (display " |--> finished")
  ;; (newline)
  (collatz-find (+ n 1)))

(collatz-find 1)
