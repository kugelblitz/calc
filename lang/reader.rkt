(module reader syntax/module-reader
  #:language 'calc/language
  #:read calc-read
  #:read-syntax calc-read-syntax
  #:whole-body-readers? #t
  #:language-info '#(calc/lang/lang-info get-info #f)
  (require calc/parser))
