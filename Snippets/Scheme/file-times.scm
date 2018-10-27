(declare (uses posix))

(define (main arguments)
  (for-each print-file-info arguments))

(define (file-time->string file-time)
  (time->string (seconds->local-time file-time)))

(define (print-file-info filename)
  (print filename ":")
  (print "  Access Time: "
         (file-time->string (file-access-time filename)))
  (print "  Modification Time: "
         (file-time->string (file-modification-time filename)))
  (print "  Change Time: "
         (file-time->string (file-change-time filename))))

(main (command-line-arguments))
