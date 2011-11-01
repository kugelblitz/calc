(module reader syntax/module-reader
  #:language 'calc/language
  #:read calc-read
  #:read-syntax calc-read-syntax
  #:whole-body-readers? #t
  (require calc/parser))
