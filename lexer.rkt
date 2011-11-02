#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide value-tokens op-tokens
         position-line position-col position-offset
         calc-lexer
         lex:comment lex:identifier lex:number lex-ci)

(define-lex-abbrevs
  (lex:letter (:or (:/ #\a #\z) (:/ #\A #\Z)))
  (lex:digit (:/ #\0 #\9))
  (lex:comment (:: "#" (:* (:: (char-complement #\newline))) (:? #\newline)))
  (lex:whitespace (:or #\newline #\return #\tab #\space #\vtab))
  (lex:identifier (:: lex:letter (:* (:or lex:letter lex:digit))))
  (lex:number (:: (:? #\-) (:+ lex:digit) (:? (:: #\. (:* lex:digit))))))

(define-tokens value-tokens (IDENTIFIER NUMBER))

(define-empty-tokens op-tokens
  (EOF ASSIGN PLUS MINUS MULTIPLY DIVIDE LEFT-PAREN RIGHT-PAREN PRINT))

(define-for-syntax (string->ci-pattern s)
  (cons ':: (map (lambda (c)
                   (list ':or (char-downcase c) (char-upcase c)))
                 (string->list s))))

(define-lex-trans lex-ci
  (lambda (stx)
    (syntax-case stx ()
      ((_ id)
       (with-syntax ((result (string->ci-pattern
                              (syntax->datum #'id))))
         #'result)))))

(define calc-lexer
  (lexer-src-pos
   ((:+ lex:whitespace) (return-without-pos (calc-lexer input-port)))
   ((:+ lex:comment) (return-without-pos (calc-lexer input-port)))
   ("=" (token-ASSIGN))
   ("+" (token-PLUS))
   ("-" (token-MINUS))
   ("*" (token-MULTIPLY))
   ("/" (token-DIVIDE))
   ("(" (token-LEFT-PAREN))
   (")" (token-RIGHT-PAREN))
   ((lex-ci "print") (token-PRINT))
   (lex:identifier (token-IDENTIFIER (string->symbol lexeme)))
   (lex:number (token-NUMBER (string->number lexeme)))
   ((eof) 'EOF)))
