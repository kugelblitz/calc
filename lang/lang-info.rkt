#lang racket/base

(provide get-info)

(define (get-info data)
  (lambda (key default)
    (case key
      ((configure-runtime)
       '(#(calc/lang/configure-runtime configure #f)))
      (else
       default))))
