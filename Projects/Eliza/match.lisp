;;; Definition of the package "match" that will contain all the necessary back-end functions.
;;; It shadows the boundp and substitute functions of the lisp package to allow a new
;;; implementation for them. This package exports the "apply-rules" function that is considered
;;; as the entry point of this package.
(defpackage match
	(:shadow lisp:boundp lisp:substitute)
	(:export apply-rules)
)
(in-package match)

;;; Defines a grammar to be used in a small parser for the english sentence "John loves Mary".
;;; It divides the sentence into N=>"John" and VP=>"loves Mary". The VP is then diveded into
;;; V=>"loves" and N=>"Mary". This grammar is used as a guide for parsing only and is not used
;;; by ELIZA.
(defparameter *grammar-rules* '((($x John $y) ($x (n John) $y))
	(($x loves $y) ($x (v loves) $y))
	(($x Mary $y) ($x (n Mary) $y))
	(($x (v $y) (n $z)) ($x (vp (v $y) (n $z))))
	(($x (v $y)) ($x (vp (v $y))))
	(((n $x) (vp $y)) (s (n $x) (vp $y))))
)

(defun variablep (x)
	"The variablep function tests if the given symbol is a variable(denoted by ? as first character)
	or not. It takes one argument as input. It returns t if the first character of x's symbol name
	is a ? and nil otherwise."
(
	and (symbolp x) (char= (char (symbol-name x) 0) #\?)substitute
))

(defun match-element (e1 e2)
	"The match-element function tests if the given two symbols matches together or not. It takes
	2 symbols as input. It returns t if the two symbols are equal or dont-care symbols, if either
	is a variable (as checked by variablep) return a list of pair that has the first element as the
	variable and the second as the other element. Returns nil otherwise."
(
	cond ((or (eql e1 e2) (dont-care e1) (dont-care e2)) t)
		((variablep e1) (cons e1 (cons e2 '())))
		((variablep e2) (cons e2 (cons e1 '())))
		(t nil)
))

(defun dont-care (x)
	"The dont-care function tests if the given symbol is the don-t care symbol "?" or not. It
	returns t if the given symbol is the question mark symbol and nil otherwise"
(
	eql x '?
))

(defun matchlelt (l1 l2)
	"The matchlelt function checks whether the given two lists matches or not. Two lists matches if
	they are of equal lengths and each constant symbols matches. The lists may contain variables or
	dont-care symbols. The input is the two lists to be matched toghether. It returns t if the two
	lists can be matched together and nil otherwise."
(
	cond ((not (eql (length l1) (length l2))) nil)
		;; The base case is that the two lists are empty.
		((and (null l1) (null l2)) t)
		;; If the first element of the first list is a list, then the first element in the second
		;; list must be a list also else return nil. Recursively, match the two sub-lists.
		((listp (first l1)) (if (listp (first l2))
			(and (matchlelt (first l1) (first l2)) (matchlelt (rest l1) (rest l2))) nil))
		;; Match the first element in each list and recursively match the rest.
		((match-element (first l1) (first l2)) (matchlelt (rest l1) (rest l2)))
		;; Otherwise, return nil.
		(t nil)
))

(defun boundp (v subs)
	"The boundp function checks whether the v symbol is bounded in the subs list or not. The input
	is the symbol v to be checked and the substitution list subs. It returns t if v is bounded in
	subs and nil otherwise."
(
	cond ((null subs) nil)
		;; If the given symbol v is the same as the first element in the first sub-list, then it
		;; is bounded.
		((eql v (first (first subs))) t)
		;; Else, try checking in the rest of the subs.
		(t (boundp v (rest subs)))
))

(defun bound-to (v subs)
	"The bound-to function returns the value that v is bounded-to in subs. It is similar to boundp
	function except that it returns the bounded value instead of boolean value. The input is the
	symbol v to be checked and the substitution list subs. It returns the bounded value of v in subs
	and nil if v is not bounded in subs"
(
	cond ((null subs) nil)
		;; If the given symbol v is the same as the first element in the first sub-list, then it
		;; is bounded and return its bounded value.
		((eql v (first (first subs))) (first (rest (first subs))))
		;; Else, try checking in the rest of the subs.
		(t (bound-to v (rest subs)))
))

(defun match1 (pat lst pairs)
	"A helper function for match that matches the pattern list to the constants list. It constructs
	the pairs list and returns it at the end of its execution. The substitution list is a list of
	pairs. The first element in the pair is the variable/sequence and the second element is the
	bounded value."
(
	cond ((null pat) pairs)
		;; If the first element in pattern is a variable and it is bounded in the result pairs, then
		;; the bounded value must be the same as the first element in the constants list. If that's
		;; the case, then recursively continue the match on the rest of the lists, otherwise return
		;; nil. If it is not-bounded, then add it to the pairs and continue match on the rest of the
		;; lists.
		((and (variablep (first pat)) (boundp (first pat) pairs))
		 	(if (eql (bound-to (first pat) pairs) (first lst))
		 		(match1 (rest pat) (rest lst) pairs) nil))
		((and (variablep (first pat)) (not (boundp (first pat) pairs)))
			(match1 (rest pat) (rest lst) (cons (cons (first pat) (cons (first lst) '())) pairs)))
		;; If the first element of the pattern is a list, then the first element of the constants
		;; list must be a list.	If that's the case, match these two sub-lists, otherwise return nil.
		((listp (first pat)) (if (listp (first lst))
			(match1 (rest pat) (rest lst) (match1 (first pat) (first lst) pairs)) nil))
		;; If the first element in pattern is a	sequence variable and it is not bounded, then use
		;; the function backtrack-match to get a bounded value for that sequence variable. If it is
		;; already bounded in pairs, then it must be the same as the first sequence of symbols in
		;; the list of constants.  If that's the case, then recursively continue the match on the
		;; rest of the lists, otherwise return nil.
		((and (svariablep (first pat)) (not (boundp (first pat) pairs)))
			(backtrack-match (first pat) (rest pat) lst '() pairs))
		((and (svariablep (first pat)) (boundp (first pat) pairs))
		 	(if (check-sequence lst (bound-to (first pat) pairs))
		 		(match1 (rest pat) (rest-sequence lst (bound-to (first pat) pairs)) pairs) nil))
		;; If the first element of the two lists are constants and not equal then return nil.
		;; Otherwise, continue the match on the rest of the lists.
		((not (eql (first pat) (first lst))) nil)
		(t (match1 (rest pat) (rest lst) pairs))
))

(defun match (pat lst)
	"General match function that takes a pattern list and another list of constants and returns the
	substitution list that maps the variables in the pattern list to their values in the constants
	list. It returns the value returned by the function match1."
(
	match1 pat lst (cons (cons t (cons t '())) '())
))

(defun substitute (pat subs)
	"The substitiute function is used to substitute the variables in the given pat with their
	bounded values in subs. It takes as input the pattern list that contains variables/sequences and
	the substition list that contains the bounded values of these variables/sequences. It returns
	the pattern list with the variables/sequences substitied by their bounded values."
(
	cond ((null pat) '())
		;; If the first element of the pattern list is a list, then apply the substition function on
		;; this sub-list followed by the rest of the original list.
		((listp (first pat))
			(append (cons (substitute (first pat) subs) '()) (substitute (rest pat) subs)))
		;; If the first element of the pattern list is a variable or a sequence and it is not
		;; bounded in the subs list, return nil.
		((and (or (svariablep (first pat)) (variablep (first pat)))
			(not (boundp (first pat) subs))) nil)
		;; If the variable/sequence is bounded in subs list, the put the bounded value and continue
		;; substituting the rest of pat list.
		((and (or (svariablep (first pat)) (variablep (first pat))) (boundp (first pat) subs))
			(append (bound-to (first pat) subs) (substitute (rest pat) subs)))
        ;; Else, then it is a constant, simply add it to the result and continue substituting the
        ;; rest of pat list.
        (t (cons (first pat) (substitute (rest pat) subs)))
))

(deftype rule ()
	"Defining the data-type rule to be a list of 2 sub-lists."
	'(satisfies rulep)
)

(defun rulep (r)
	"The function rulep checks that the given argument r is a rule. A rule is defined to be a list
	of 2 sub-lists."
	(and (listp r)
	(= (length r) 2)
	(listp (first r))
	(listp (second r))
))

(defun rhs (r)
	"Returns the right hand side list of the given rule r."
	(check-type r rule)
	(second r)
)

(defun lhs (r)
	"Returns the left hand side list of the given rule r."
	(check-type r rule)
	(first r)
)

(defun apply-rule (tree rule)
	"The function apply-rule is used to apply the given rule on the given list tree and returns the
	result of the rule. It takes as input the rule to be used and the list to be applied. If the
	list tree matches with the left-hand-side list of given rule, it applies the right-hand-side
	list of the given rule on the given list. Else, it return the original list unaffected."
(
	if (match (lhs rule) tree) (substitute (rhs rule) (match (lhs rule) tree)) tree
))

(defun apply-rules (tree rule-list)
	"The function apply-rules applies the first rule in the list RULE-LIST to the given list tree,
	the second rule to the result of applying the first, and so on, and return the result of
	applying the last rule to the results of the second-to-last"
(
	cond ((null rule-list) tree)
		 (t (apply-rules (apply-rule tree (first rule-list)) (rest rule-list)))
))

(defun svariablep (s)
	"The svariablep function tests if the given symbol s is a sequence variable(denoted by $ as
	first character) or not. It takes one argument as input. It returns t if the first character
	of s's symbol name is a $ and nil otherwise."
(
	and (symbolp s) (char= (char (symbol-name s) 0) #\$)
))

(defun backtrack-match (v pat tree sqce pairs)
	"The backtrack-match function is used to find a suitable value for a given sequence variable v
	in the given tree. The given pat list is the rest of the pattern passed from the match function.
	The function uses the sqce list as an intermediate list to hold the result. The given pairs list
	is the already substition list found in the match function. It returns the suitable sequence of
	symbols that are to be bounded to the sequence variable v."
(
	cond
		;; If there pattern list is empty, then v will be bounded to all the tree.
		((null pat) (cons (list v (append sqce tree)) pairs))
		;; Check if the already founded sqce, allows the list pat and list tree matches or not.
		((match1 pat tree (cons (list v sqce) pairs)))
		;; If not and the tree list is empty, then return nil.
		((null tree) nil)
		;; Otherwise, try adding one symbol to already founded sqce.
		(t (backtrack-match v pat (rest tree) (append sqce (list (first tree))) pairs))
))

(defun check-sequence (lst bound)
	"The check-sequence function is used to make sure that the bound elements are the first elements
	in the lst list."
(
	cond ((null bound) t)
		 ((null lst) nil)
		 ((eql (first lst) (first bound)) (check-sequence (rest lst) (rest bound)))
		 (t nil)
))

(defun rest-sequence (lst bound)
	"The rest-sequence function is used to get the rest of the list lst after deleting the bound
	elmenets from its start. It must be called after checking that check-sequence function returns
	true"
(
	cond ((null bound) lst)
		 ((rest-sequence (rest lst) (rest bound)))
		 (t nil)
))
