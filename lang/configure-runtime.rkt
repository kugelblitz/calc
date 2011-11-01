#lang racket/base

(require calc/parser)
(provide configure)

(define (configure data)
  (current-read-interaction even-read))

(define (even-read source-name input-port)
  (begin0
    (parse-calc-port input-port source-name)
    (current-read-interaction odd-read)))

(define (odd-read src ip)
  (current-read-interaction even-read)
  eof)
