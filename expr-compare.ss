(define (expr-compare x y)
  (cond 
    ((equal? x y) x)
    ((and (boolean? x) (boolean? y))
      (if x
        '%
        `(not %)))
    (else '(if %, x, y))))
