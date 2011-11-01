#lang racket

(require calc/lexer
         parser-tools/lex)
(provide repl-submit?)

(define (repl-submit? ip has-white-space?)
  (let loop ((blank? #t)
             (pending-op? #f)
             (paren-count 0))
    (with-handlers ((exn:fail:read?
                     (lambda (e)
                       #t)))
      (let ((token (position-token-token (calc-lexer ip))))
        (case token
          ((EOF)
           (and (zero? paren-count)
                (not blank?)
                (not pending-op?)))
          ((PLUS MINUS MULTIPLY DIVIDE ASSIGN PRINT)
           (loop #f #t paren-count))
          ((LEFT-PAREN)
           (loop #f #f (+ paren-count 1)))
          ((RIGHT-PAREN)
           (loop #f #f (- paren-count 1)))
          (else
           (loop #f #f paren-count)))))))
