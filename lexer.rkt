#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide value-tokens op-tokens
         position-line position-col position-offset
         calc-lexer)

(define-lex-abbrevs
  (lex:letter (:or (:/ #\a #\z) (:/ #\A #\Z)))
  (lex:digit (:/ #\0 #\9))
  (lex:whitespace (:or #\newline #\return #\tab #\space #\vtab)))

(define-tokens value-tokens (IDENTIFIER NUMBER))

(define-empty-tokens op-tokens
  (EOF ASSIGN PLUS MINUS MULTIPLY DIVIDE LEFT-PAREN RIGHT-PAREN PRINT))

(define calc-lexer
  (lexer-src-pos
   ((:+ lex:whitespace) (return-without-pos (calc-lexer input-port)))
   ("=" (token-ASSIGN))
   ("+" (token-PLUS))
   ("-" (token-MINUS))
   ("*" (token-MULTIPLY))
   ("/" (token-DIVIDE))
   ("(" (token-LEFT-PAREN))
   (")" (token-RIGHT-PAREN))
   ("print" (token-PRINT))
   ((:: lex:letter (:* (:or lex:letter lex:digit)))
    (token-IDENTIFIER (string->symbol lexeme)))
   ((:: (:? #\-) (:+ lex:digit) (:? (:: #\. (:* lex:digit))))
    (token-NUMBER (string->number lexeme)))
   ((eof) 'EOF)))
