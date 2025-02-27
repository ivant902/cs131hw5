#lang racket
(define (expr-compare x y)
(cond
  ;; if x and y are just equal
  ((equal? x y) x) 
  ;; if x and y are both booleans
  ((and (boolean? x) (boolean? y)) (if x '% '(not %)))
  ;; if x or y are not lists
  ((or (not(list? x)) (not(list? y))) (list 'if '% x y))
  ;; if x and y are lists but not equal in length
  ((and (list? x) (list? y) (not (equal? (length x) (length y)))) (list 'if '% x y))
  ;; if x and y are lists and equal in length
  ((and (list? x) (list? y) (equal? (length x) (length y))) (expr-compare-lists x y))
)) 
; uses member to check if lambda is in the expression 
(define (lambda? x) (member x '(lambda λ)))

;basic checker 
(define (expr-compare-lists x y)
  (cond
    ; either x or y starts with if
    ((or (equal? (car x) 'if) (equal? (car y) 'if)) (list 'if '% x y))
    ; both start with if
    ((and (equal? (car x) 'if) (equal? (car y) 'if)) (expr-compare-other x y))
    ; check for starting quotes for EITHER
    ((or (equal? (car x) 'quote) (equal? (car y) 'quote)) (list 'if '% x y))
    ; check for BOTH x and y starting with lambda quote
    ((and (equal? (car x) 'lambda) (equal? (car y) 'lambda))
      (cond 
        ((equal? (length (car (cdr x))) (length (car (cdr y)))) (expr-compare-lambda (cdr x) (cdr y) 'lambda '() '()))
        (else (list 'if '% x y))
      ))
    ; check for BOTH x and y starting with lambda symbol
    ((and (equal? (car x) 'λ) (equal? (car y) 'λ))
      (cond 
        ((equal? (length (car (cdr x))) (length (car (cdr y)))) (expr-compare-lambda (cdr x) (cdr y) 'λ '() '()))
        (else (list 'if '% x y))
      ))
    ; check for same vs diff lambda symbols
    ((and (lambda? (car x)) (lambda? (car y)))
      (cond 
        ((equal? (length (car (cdr x))) (length (car (cdr y)))) (expr-compare-lambda (cdr x) (cdr y) 'λ '() '()))
        (else (list 'if '% x y))
      ))
    ; check for only a single lambda symbol
    ((or (lambda? (car x)) (lambda? (car y))) (list 'if '% x y))
    (else (expr-compare-other x y))
))
;; comparing non-lambda functions 
(define (expr-compare-other x y)
  (cond 
  ; base case
  ((and (empty? x) (empty? y)) '())
  ; boolean case -- if x is true then % if false then not % then recursively call 
  ((and (boolean? (car x)) (boolean? (car y))) (cons (if (car x) '% '(not %)) (expr-compare-other (cdr x) (cdr y))))
  ; case where top element in list are equal so just add to list and recursively call rest of list
  ((equal? (car x) (car y)) (cons (car x) (expr-compare-other (cdr x) (cdr y))))
  ; case where top element in list aren't equal
  (else (cond 
    ; case where both elements are same length and are lists
    ; pass the first elements to the basic checker and the rest of the lists to recursively call
    ((and (list? (car x)) (list? (car y)) (equal? (length (car x)) (length (car y)))) 
      (cons (expr-compare-lists (car x) (car y)) (expr-compare-other (cdr x) (cdr y))))
    ; case where both elements are not same length and are lists
    ((and (list? (car x)) (list? (car y)))
      (cons (list 'if '% (car x) (car y)) (expr-compare-other (cdr x) (cdr y))))
    ; rest of cases
    (else (cons (list 'if '% (car x) (car y)) (expr-compare-other (cdr x) (cdr y))))
  ))
))

(define (expr-compare-lambda x y lambda a b)
  (list 'if '% x y)
)

(define (test-expr-compare x y)
(and
 (equal? (eval x) (eval (list 'let '([% #t]) (expr-compare x y))))
 (equal? (eval y) (eval (list 'let '([% #f]) (expr-compare x y))))
 )
)
