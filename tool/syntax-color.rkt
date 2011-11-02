#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         calc/lexer)

(provide get-syntax-token)

(define (syn-val lexeme type paren start end)
  (values lexeme type paren (position-offset start) (position-offset end)))

(define get-syntax-token
  (lexer
   ((:+ whitespace)
    (syn-val lexeme 'whitespace #f start-pos end-pos))
   (lex:comment
    (syn-val lexeme 'comment #f start-pos end-pos))
   (lex:number
    (syn-val lexeme 'constant #f start-pos end-pos))
   ((lex-ci "print")
    (syn-val lexeme 'keyword #f start-pos end-pos))
   (lex:identifier
    (syn-val lexeme 'symbol #f start-pos end-pos))
   ((:or #\+ #\- #\/ #\* #\=)
    (syn-val lexeme 'parenthesis #f start-pos end-pos))
   (#\( (syn-val lexeme 'parenthesis '|(| start-pos end-pos))
   (#\) (syn-val lexeme 'parenthesis '|)| start-pos end-pos))
   ((eof) (syn-val lexeme 'eof #f start-pos end-pos))
   (any-char (syn-val lexeme 'error #f start-pos end-pos))))
