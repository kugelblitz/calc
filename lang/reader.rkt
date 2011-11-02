(module reader syntax/module-reader
  #:language 'calc/language
  #:read calc-read
  #:read-syntax calc-read-syntax
  #:whole-body-readers? #t
  #:language-info '#(calc/lang/lang-info get-info #f)
  #:info (lambda (key defval default)
           (case key
             [(color-lexer)
              (dynamic-require 'calc/tool/syntax-color 'get-syntax-token)]
             [else (default key defval)]))

  (require calc/parser))
