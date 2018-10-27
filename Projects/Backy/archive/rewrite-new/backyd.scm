(use getopt-long)

(define getopt-grammar
  `((safe    (required #f) (single-char #\s) (value #f))
    (detach  (required #f) (single-char #\d) (value #f))
    (port    (required #t) (single-char #\p)
             (value (required PORT)
                    (transformer ,string->number)))
    (address (required #t) (single-char #\a)
             (value (required ADDR)
                    (predicate ,string?)))))

(condition-case (getopt-long (argv) getopt-grammar)
  [e (exn) (print (get-condition-property e 'exn 'message))])

;; (getopt-long (argv) getopt-grammar)
