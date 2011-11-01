#lang racket

(provide compile-program
         compile-statement)

(define variables (make-hash))

(define (compile-program p)
  (for-each check-syntax (syntax-e p))
  p)

(define (compile-statement s)
  (check-syntax s)
  s)

(define (check-syntax s)
  (match (syntax->datum s)
    ((list 'assign a b)
     (check-syntax (third (syntax-e s)))
     (hash-set! variables a a))
    ((list 'value-of a)
     (unless (hash-has-key? variables a)
       (raise-syntax-error
        #f "access to unassigned variable" (second (syntax-e s)))))
    ((list x ...)
     (for-each check-syntax (syntax-e s)))
    (_ #f)))
