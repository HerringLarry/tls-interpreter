(define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))

(define (build s1 s2) (cons s1 (cons s2 '())))

;; an entry is a pair of lists whose first list is a set.
;; and the twor lists must be of equal length. (k-v)
(define new-entry build)
(define (lookup-in-entry name entry entry-f)
	(define (lookup-in-entry-help name names values entry-f)
		(cond
			((null? names) (entry-f name))
			((eq? name (car names)) (car values))
			(else (lookup-in-entry-help name 
					(cdr names)
					(cdr values)
					entry-f))))
	(lookup-in-entry-help
		name
		(first entry)
		(second entry)
		entry-f))

;; a table(also called environment) is a list of entries.

;; creating a new table by putting the new entry in front of the old table.
(define extend-table cons)

;; mention the trick in this function!!!!
(define (lookup-in-table name table table-f)
	(if (null? table)
		(table-f name)
		(lookup-in-entry 
			name
			(car table)
			(lambda (name)
				(lookup-in-table
					name
					(cdr table)
					table-f)))))
;;;;;
(define value
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))


(define expression-to-action
  (lambda (e)
    (if (atom? e)
        (atom-to-action e)
        (list-to-action e))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e 'cons) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'null?) *const)
      ((eq? e 'atom?) *const)
      ((eq? e 'eq?) *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'add1) *const)
      ((eq? e 'sub1) *const)
      ((eq? e 'number?) *const)
      ((eq? e '+) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (if (atom? (car e))
        (cond 
           ((eq? (car e) 'quote) *quote)
           ((eq? (car e)'lambda) *lambda)
           ((eq? (car e) 'cond) *cond)
           (else *application))
        *application)))

(define *const
  (lambda (e table)
    (cond ((number? e) e)
          ((eq? e #t) #t)
          ((eq? e #f) #f)
          (else (build 'primitive e)))))

(define *quote
  (lambda (e table)
    (text-of e)))
(define first car)
(define second cadr)
(define third caddr)

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))
(define initial-table
  (lambda (name) '()))
       
(define *lambda
  (lambda (e table)
    (build 'non-primitive (cons table (cdr e)))))

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines))
                table))
      ((meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))
(define question-of first)
(define answer-of second)

(define else?
  (lambda (x)
    (if (atom? x)
        (eq? x 'else)
        #f)))

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))
(define cond-lines-of cdr)

;; eval the values of arguments which is a list
(define evlis
  (lambda (args table)
	(if (null? args) 
		'()
        (cons (meaning (car args) table) 
			  (evlis (cdr args) table)))))

(define *application
  (lambda (e table)
    (new-apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))
(define function-of car)
(define arguments-of cdr)

(define new-apply
  (lambda (fun vals)
    (cond
      ((new-primitive? fun)
       (apply-primitive
        (second fun) vals))
      ((non-primitive? fun)
       (apply-closure
        (second fun) vals)))))

(define new-primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))
(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name 'cons)
       (cons (first vals) (second vals)))
      ((eq? name 'car)
       (car (first vals)))
      ((eq? name 'cdr)
       (cdr (first vals)))
      ((eq? name 'null?)
       (null? (first vals)))
      ((eq? name 'eq?)
       (eq? (first vals) (second vals)))
      ((eq? name 'atom?)
       (atom? (first vals)))
      ((eq? name 'zeros?)
       (zeros (first vals)))
      ((eq? name 'add1)
       (add1 (first vals)))
      ((eq? name 'sub1)
       (sub1 (first vals)))
      ((eq? name 'number?)
       (number? (first vals)))
      ((eq? name '+)
       (+ (first vals) (second vals))))))

(define (add1 x) (+ x 1))
(define (sub1 x) (- x 1))

;; eval the value of lambda function. closure is consisted of (table arguments function-body)
;; vals is the value of arguments.
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
			   (formals-of closure)
               vals)
              (table-of closure)))))

(define table-of first)
(define formals-of second)
(define body-of third)

(value '((lambda (x) (+ x 1)) 3))

(value '((lambda (x) (add1 x)) 3))

(value '((lambda (x z)
           (cons x
                 ((lambda (x y) (cons z (cons x  y)))
                  3 4)
                 ))
         1 2))
