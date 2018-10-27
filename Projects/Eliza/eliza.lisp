;;; Definition of the package "eliza" that will contain the main function which is the entry point
;;;	for the eliza application. It uses the common-lisp functions and imports the apply-rules
;;;	function from the match package.
(defpackage eliza
	(:use common-lisp)
	(:import-from match apply-rules)
)
(in-package eliza)

(defparameter *rules*
	'(
		(($x alike) (In what way?))
		(($x something or other) (Can youu think of a specific example?))
		(($x are like $y) (what resemblance do youu see?))
		(($x is like $y) (what resemblance do youu see?))

		(($x my $y) ($x your $y))
		(($x me $y) ($x youm $y))
		(($z your $x made youm $y) (your $x made youu $y))
		(($x you are $y) ($x youu aree $y))
		(($x you $y) ($x youi $y))
		((youu aree $x but $y) (what makes you think ii amm $x))

		(($x I $y) ($x you $y))
		(($x am $y) ($x are $y))
		((you need $x that $y) (what would it mean to you if you got $x))
		((you need $x) (what would it mean to you if you got $x))
		(($x you $y learn $z your mother) (tell me more about your family))
		(($x you $y learn $z your father) (tell me more about your family))
		(($x you $y learn $z your brother) (tell me more about your family))
		(($x you $y learn $z your sister) (tell me more about your family))
		((your mother $x of youm) (who else in your family $x of you?))
		((your brother $x of youm) (who else in your family $x of you?))
		((your father $x of youm) (who else in your family $x of you?))
		((your sister $x of youm) (who else in your family $x of you?))

		((he says you are $x much of the time) (i am sorry to hear you are $x))
		((he says you are $x most of the time) (i am sorry to hear you are $x))
		(($x you are unhappy $y) (do you think coming here will help you not to be unhappy?))

		((youi $x with youm $y) (why do youu think ii $x with you))
		((youu aree $x youm) (does it please you to believe ii amm $x you))
		((your $x is $y) (what else comes to mind when you think of your $x))
		((bullies) (does that have anything to do with the fact that you are here))

		(($x but $y) ($x))
		(($x and $y) ($x))
		(($x deadlines  $y) (do deadlines make you worry))
		(($x deadline $y) (is it hard to cope up with all the deadlines))
		(($x you are tired of $y but $z) (why are youu tired of $y))
		(($x you are tired of $y) (why are youu tired of $y))
		(($x lisp is $y)(how is lisp $y))
		(($x takes all your energy $y) (do you think you would do better off without $x))
		(($x you worry about $y) (is $y important to you))
		(($x your $y make youm feel like you have to $z) (do you care what your $y make you feel))
		(($x sometimes $y) (when was the last time?))
		(($x youi think $z guc $y) (ii amm a guc counselor so i am not allowed to comment on that))
		((what do $z think $y) (ii amm not sure what $z think))
		((what does $z think $y) (ii amm not sure what $z thinks))
		(($x yes you $y do $z) (then its probably true))
		(($x you have to go now $y) (good luck then))
	)
)

(defparameter *secondary-rules*
	'(
		(($x youu $y) ($x you $y))
		(($x youm $y) ($x you $y))
		(($x youi $y) ($x i $y))
		(($x aree $y) ($x are $y))
		(($x ii $y) ($x i $y))
		(($x amm $y) ($x am $y))
	)
)

(defun main ()
	"The main function of the application. It consits of an infinte loop that reads the input the
	user, apply the rules on this input and finally prints the output. The user will end the
	conversation by entering bye."
(
	loop
		(let
		 	((input (readasentence)))
		 	;; check if the user wants to end the conversation, then terminate from the loop.
		 	(when (string= (symbol-name (first input)) "BYE") (return 'good-bye))
		 	;; apply the rules on the input, first rules then the secondary-rule list
		 	;; and formates the output list to be printed. The format is to print the elements
			;; of the list.
			(format t "~{~A ~}" (apply-rules (apply-rules input *rules*) *secondary-rules*))
			(princ #\newline)
		)
))

(defun terminatorp (symb)
	"The function terminatorp is used to check if given symb is a sentence terminator or not. The
	checked characters are the '.', '?' and '!'."
	(check-type symb symbol)
	(let
		;; Initializes the symbstr variable to be the last character in the given symbol's name
		((symbstr (char (symbol-name symb) (1- (length (symbol-name symb))))))
		(or
			(char= symbstr #\.)
			(char= symbstr #\?)
			(char= symbstr #\!)
		)
	)
)

(defun readasentence ()
	"Reads a sequence of S-expressions until a terminator is encountered. Returns a list of the
	 expressions without the terminator."
(
	let (backwards)
    (loop
    	;; Read a new expression and push it on the backwards list.
    	(push (read) backwards)
    	;; if the terminator is encountered, then add a new symbol whose name is the same as the
    	;; last input expression but without the terminator symbol. Return the reverse of the
    	;; backward list.
		(when (terminatorp (first backwards)) (return (reverse (cons (intern (subseq
			(symbol-name (first backwards)) 0 (1- (length (symbol-name (first backwards))))))
				(rest backwards)))))
	)
))
