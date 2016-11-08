;; ----------- Part 1 begins ---------------
(define pets
        '((cat 1) (dog 1) (fish 1) (cat 2) (fish 2))
)

(define is-pair?
	(lambda (x)
		(cond
			((null? x)
				#f)
			((list? x)
				(cond 
					((equal? (length x) 2)
						#t)
					(else 
						#f)
				))
			(else
				#f)
		)
	)
)

(define is-alist?
	(lambda (x)
		(cond 
			((null? x)
				#t)
			((list? x)
				(cond 
					((is-pair? (car x))
						(is-alist? (cdr x)))
					(else 
						#f)
				))
			(else
				#f)
		)
	)
)

(define get-all-pairs
	(lambda (key lst)
		(cond
			((null? lst)
				'())
			((is-alist? lst)
				(cond
					((equal? key (car(car lst)))
						(cons (car lst) (get-all-pairs key (cdr lst))))
					(else
						(get-all-pairs key (cdr lst)))
				))
			(else
				(error "lst is not an association list"))
		)
	)
)

(define get-first-pair
	(lambda (key lst)
		(cond
			((null? lst)
				'())
			((is-alist? lst)
				(cond
					((equal? key (car(car lst)))
						(car lst))
					(else
						'())
				))
			(else
				(error "lst is not an association list"))
		)
	)
)

(define del-all-pairs
	(lambda (key lst)
		(cond 
			((null? lst)
				'())
			((is-alist? lst)
				(cond 
					((equal? key (car(car lst)))
						(del-all-pairs key (cdr lst)))
					(else
						(cons (car lst) (del-all-pairs key (cdr lst))))
				))
			(else
				(error "lst is not an association list"))
		)
	)
)

(define del-first-pair 
	(lambda (key lst)
		(cond
			((null? lst)
				'())
			((is-alist? lst)
				(cond
					((equal? key (car(car lst)))
						(cdr lst))
					(else
						lst)
				))
			(else
				(error "lst is not an association list"))
		)
	)
)

;; ----------- Part 1 ends ---------------

;; ----------- Part 2 begins ---------------
(define exponent
	(lambda (left right)
		(cond
			((equal? right 0)
				1)
			(else
				(* left (exponent left (- right 1))))
		)
	)
)

(define expression
	(lambda (e)
		(cons
			((number? e)
				e)
			((symbol? e)
				(get-value e))
			;; (left operator right)
			(else
				(let ((left (expression (first e)))
				   (operator (second e))
				   (right (expression (third e))))
					(cond
						((equal? operator '+) 
							(+ left right))
						((equal? operator '-) 
							(- left right))
						((equal? operator '*) 
							(* left right))
						((equal? operator '/) 
							(/ left right))
						((equal? operator '**)
							(exponent left right))
						(else
							(error "unknown operator"))
					)
				)
			)
		)
	)
)

(define myeval
	(lambda (expr envir)
		(cond 
			((is-alist? envir)
				)
			(else	
				(error "environment is not an association list"))
		)
	)
)




























;; ----------- Part 2 ends ---------------