#lang racket/base

(provide get-info)

(define (get-info data)
  (lambda (key default)
    (case key
      ((configure-runtime)
       '(#(calc/lang/configure-runtime configure #f)))
      ((drracket:submit-predicate)
       (dynamic-require 'calc/tool/submit 'repl-submit?))
      (else
       default))))
