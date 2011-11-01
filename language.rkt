#lang racket

(provide
 #%module-begin #%datum
 assign plus minus divide multiply negate value-of print)

(define current-env (make-hash))

(define-syntax-rule (assign name value)
  (hash-set! current-env 'name value))

(define-syntax-rule (plus a b)
  (+ a b))

(define-syntax-rule (minus a b)
  (- a b))

(define-syntax-rule (divide a b)
  (/ a b))

(define-syntax-rule (multiply a b)
  (* a b))

(define-syntax-rule (negate a)
  (- a))

(define-syntax-rule (value-of name)
  (hash-ref current-env 'name))

(define-syntax-rule (print value)
  (printf "~v\n" value))
