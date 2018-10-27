;; Ahmad Hisham, Mohammed Al Mehdar, Fred Morcos

(defstruct (FOLTerm
(:print-function
               (lambda (struct stream depth)
                 (declare (ignore depth))
                 (format stream "[~A ~A ~A]"
                         (FOLTerm-isPositive struct)
                         (FOLTerm-predicateName struct)
                         (FOLTerm-symbolsList struct))))
) isPositive predicateName symbolsList)

(defstruct action preConds name effects)
(defstruct link LHS RHS preCond)
(defstruct plan actions links orderings)

(defparameter plan nil)

(defun makeMinimalPlan (start finish)
	(make-plan :actions (cons start (cons finish '()))
					   :links '()
					   :orderings (cons (make-link :LHS 'Start :RHS 'Finish) '())
	)
)

(defun preCondIsComplete (preCond)
	(dotimes (i (length (plan-links plan)))
		(setf l (nth i (plan-links plan)))
		(if (eql (link-RHS l) preCond) 't)
	)
	preCond
)

(defun actionIsComplete (action)
	(dotimes (i (length (action-preConds action)))
		(setf o (nth i (action-preConds action)))
		(setf res (preCondIsComplete o))
		(if (not (null res)) (return-from actionIsComplete res))
	)
)

(defun isSolution ()
	(dotimes (i (length (plan-actions plan)))
		(setf a (nth i (plan-actions plan)))
		(setf res (actionIsComplete a))
		(if (not (null res)) (return-from isSolution res))
	)
)

(defun getOps (preCond operators)
	(setf res '())
	(dotimes (i (length operators))
		(setf o (nth i operators))
		(dotimes (j (length (action-effects o)))
			(setf e (nth j (action-effects o)))
			(princ preCond)
			(princ e)
			(setf subs (match preCond e))
;;			(princ subs)
;;			(if (not (null subs)) (cons (substitute subs o) res))
		)
	)
	res
)

(defun variablep (x)
	"The variablep function tests if the given symbol is a variable(denoted by ? as first character)
	or not. It takes one argument as input. It returns t if the first character of x's symbol name
	is a ? and nil otherwise."
(
	and (symbolp x) (char= (char (symbol-name x) 0) #\?)
))

(defun match (cond1 cond2)
	(setf l1 (FOLTerm-symbolsList cond1))
	(setf l2 (FOLTerm-symbolsList cond2))
	(setf res '())
	(if (and (predEql cond1 cond2) (eql (length l1) (length l2)))
			(dotimes (i (length l2))
				(setf tmp (nth i l2))
				(if (variablep tmp) (if (alreadyAssigned tmp (nth i l1) res)
										(setf res(cons (cons tmp (cons (nth i l1) '()))
											res))
										(return-from alreadyAssigned nil))
									)
			)
	)
	res
)

(defun alreadyAssigned (var const list)
	(dotimes (i (length list))
		(if (and (eql var (first (nth i list))) (not (eql const (second (nth i list))))) (return-from alreadyAssigned nil))
	)
	't
)

(defun predEql (cond1 cond2)
	(if (and (eql (FOLTerm-isPositive cond1) (FOLTerm-isPositive cond2))
			 (eql (FOLTerm-predicateName cond1) (FOLTerm-predicateName cond2)))
		t
		nil
	)
)

(defun getPlan (start finish operators)
	(setf plan (makeMinimalPlan start finish))
;;	(loop
		(setf subGoal (isSolution))
		(princ subGoal)
;;		(when (null subGoal) (return p))
		(getOps subGoal operators)
;;	)
)

(defun main ()
	(setf start (make-action :preConds '()
							 :name (make-FOLTerm :isPositive 't :predicateName 'Start :symbolsList '())
							 :effects '()
	))

	(setf (action-effects start) (cons (make-FOLTerm :isPositive 't :predicateName 'At :symbolsList '(Home))
		(action-effects start)))
	(setf (action-effects start) (cons (make-FOLTerm :isPositive 't :predicateName 'Sells :symbolsList '(HWS Drill))
		(action-effects start)))
	(setf (action-effects start) (cons (make-FOLTerm :isPositive 't :predicateName 'Sells :symbolsList '(SM Milk))
		(action-effects start)))
	(setf (action-effects start) (cons (make-FOLTerm :isPositive 't :predicateName 'Sells :symbolsList '(SM Banana))
		(action-effects start)))

	(setf finish (make-action :preConds '()
							  :name (make-FOLTerm :isPositive 't :predicateName 'Finish :symbolsList '())
						 	  :effects '()
	))

	(setf (action-preConds finish) (cons (make-FOLTerm :isPositive 't :predicateName 'At :symbolsList '(Home))
		(action-preConds finish)))
	(setf (action-preConds finish) (cons (make-FOLTerm :isPositive 't :predicateName 'Have :symbolsList '(Drill))
		(action-preConds finish)))
	(setf (action-preConds finish) (cons (make-FOLTerm :isPositive 't :predicateName 'Have :symbolsList '(Milk))
		(action-preConds finish)))
	(setf (action-preConds finish) (cons (make-FOLTerm :isPositive 't :predicateName 'Have :symbolsList '(Banana))
		(action-preConds finish)))

	(setf go (make-action :preConds '()
							  :name (make-FOLTerm :isPositive 't :predicateName 'Go :symbolsList '(?there))
						 	  :effects '()
	))

	(setf (action-preConds go) (cons (make-FOLTerm :isPositive 't :predicateName 'At :symbolsList '(?here))
		(action-preConds go)))
	(setf (action-effects go) (cons (make-FOLTerm :isPositive 't :predicateName 'At :symbolsList '(?there))
		(action-effects go)))
	(setf (action-effects go) (cons (make-FOLTerm :isPositive 'n :predicateName 'At :symbolsList '(?here))
		(action-effects go)))

	(setf buy (make-action :preConds '()
							  :name (make-FOLTerm :isPositive 't :predicateName 'Buy :symbolsList '(?x))
						 	  :effects '()
	))

	(setf (action-preConds buy) (cons (make-FOLTerm :isPositive 't :predicateName 'At :symbolsList '(?store))
		(action-preConds buy)))
	(setf (action-preConds buy) (cons (make-FOLTerm :isPositive 't :predicateName 'Sells :symbolsList '(?store ?x))
		(action-preConds buy)))
	(setf (action-effects buy) (cons (make-FOLTerm :isPositive 't :predicateName 'Have :symbolsList '(?x))
		(action-effects buy)))

	(setf operators (cons go (cons buy '())))

	(getPlan start finish operators)

;;	(linearize p)
)
