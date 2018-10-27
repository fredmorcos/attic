(defvar *expenses* nil)

(defun make-expense (amount date-year date-month date-day
                     person-name shop-name tags note)
  (declare (float amount) (fixnum date-year) (fixnum date-month)
           (fixnum date-day) (string person-name)
           (string shop-name) (list tags) (string note))
  (list :amount amount :date-year date-year :date-month  date-month
        :date-day date-day :person-name person-name
        :shop-name shop-name :tags tags :note note))

(defun add-expense (expense)
  (push expense *expenses*))

(defun dump-expenses ()
  (format t "~{~{~a:~15t~a~%~}~%~}" *expenses*))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-read-tags ()
  (let ((tag (prompt-read "Tag")))
    (cons tag
          (if (y-or-n-p "Add another tag?")
              (prompt-read-tags)
              nil))))

(defun prompt-expense ()
  (make-expense
   (read-from-string (prompt-read "Amount"))
   (or (parse-integer (prompt-read "Year")  :junk-allowed t) 0)
   (or (parse-integer (prompt-read "Month") :junk-allowed t) 0)
   (or (parse-integer (prompt-read "Day")   :junk-allowed t) 0)
   (prompt-read "Person")
   (prompt-read "Shop")
   (prompt-read-tags)
   (prompt-read "Note")))

(defun add-expenses ()
  (loop (add-expense (prompt-expense))
     (if (not (y-or-n-p "Add another expense?")) (return))))

(defun save-expenses (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax (print *expenses* out))))

(defun load-expenses (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax (setf *expenses* (read in)))))

(defun select-by-field (field value)
  (remove-if-not #'(lambda (expense)
                     (equal (getf expense field) value))
                 *expenses*))

(defun select (selector-fn)
  (remove-if-not selector-fn *expenses*))

;; (defun where (&key amount date-year date-month date-day
;;                 person-name shop-name tags note)
;;   #'(lambda (expense)
;;       (and
;;        (if amount      (equal (getf expense :amount)      amount)      t)
;;        (if date-year   (equal (getf expense :date-year)   date-year)   t)
;;        (if date-month  (equal (getf expense :date-month)  date-month)  t)
;;        (if date-day    (equal (getf expense :date-day)    date-day)    t)
;;        (if person-name (equal (getf expense :person-name) person-name) t)
;;        (if shop-name   (equal (getf expense :shop-name)   shop-name)   t)
;;        (if tags        (equal (getf expense :tags)        tags)        t)
;;        (if note        (equal (getf expense :note)        note)        t))))

(defun update (selector-fn &key amount date-year date-month date-day
                             person-name shop-name tags note)
  (setf
   *expenses*
   (mapcar
    #'(lambda (expense)
        (when (funcall selector-fn expense)
          (if amount      (setf (getf expense :amount)      amount))
          (if date-year   (setf (getf expense :date-year)   date-year))
          (if date-month  (setf (getf expense :date-month)  date-month))
          (if date-day    (setf (getf expense :date-day)    date-day))
          (if person-name (setf (getf expense :person-name) person-name))
          (if shop-name   (setf (getf expense :shop-name)   shop-name))
          (if tags        (setf (getf expense :tags)        tags))
          (if note        (setf (getf expense :note)        note)))
        expense) *expenses*)))

(defun delete-expense (selector-fn)
  (setf *expenses* (remove-if selector-fn *expenses*)))

(defun make-comparison-expr (field value)
  `(equal (getf expense ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
       collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (expense) (and ,@(make-comparisons-list clauses))))
