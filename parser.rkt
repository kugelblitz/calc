#lang racket

(require parser-tools/yacc
         syntax/readerr
         calc/lexer)

(provide calc-read-syntax
         calc-read)

(define (on-error source-name)
  (lambda (tok-ok? tok-name tok-value start-pos end-pos)
    (raise-read-error 
             "Parser error"
             source-name
             (position-line start-pos)
             (position-col start-pos)
             (position-offset start-pos)
             (- (position-offset end-pos)
                (position-offset start-pos)))))

(define (calc-parser source-name)
  (parser
   (src-pos)
   (start start)
   (end EOF)
   (tokens value-tokens op-tokens)
   (error (on-error source-name))
   
   (grammar
    (start
     ((statements) $1))
    
    (statements
     (() '())
     ((statement statements) (list* $1 $2)))
    
    (statement
     ((assignment) $1)
     ((printing) $1))
    
    (constant
     ((NUMBER) $1))
    
    (expression
     ((term) $1)
     ((expression PLUS term) (list 'plus  $1 $3))
     ((expression MINUS term) (list 'minus $1 $3)))
    
    (term
     ((factor) $1)
     ((term MULTIPLY factor) (list 'multiply $1 $3))
     ((term DIVIDE factor) (list 'divide $1 $3)))
    
    (factor
     ((primary-expression) $1)
     ((MINUS primary-expression) (list 'negate $2))
     ((PLUS primary-expression) $2))
    
    (primary-expression
     ((constant) $1)
     ((IDENTIFIER) (list 'value-of $1))
     ((LEFT-PAREN expression RIGHT-PAREN) $2))
    
    (assignment
     ((IDENTIFIER ASSIGN expression) (list 'assign $1 $3)))
    
    (printing
     ((PRINT expression) (list 'print $2))))))

(define (parse-calc-port port file)
  (port-count-lines! port)
  ((calc-parser file)
   (lambda () 
     (calc-lexer port))))

(define (calc-read in)
  (syntax->datum
   (calc-read-syntax #f in)))

(define (calc-read-syntax source-name input-port)
  (parse-calc-port input-port source-name))