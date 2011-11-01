(module reader syntax/module-reader
  #:language 'racket
  #:read calc-read
  #:read-syntax calc-read-syntax
  #:whole-body-readers? #t
  (require calc/parser))
