#lang racket/base

(require calc/parser
         calc/compiler)
(provide configure)

(define (configure data)
  (current-read-interaction even-read))

(define (even-read source-name input-port)
  (begin0
    (compile-statement (parse-calc-port input-port source-name))
    (current-read-interaction odd-read)))

(define (odd-read src ip)
  (current-read-interaction even-read)
  eof)
