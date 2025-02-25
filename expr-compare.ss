(define (expr-compare x y)
  (cond 
    ((equal? x y) x)
    (else `(if % ,x ,y))))
