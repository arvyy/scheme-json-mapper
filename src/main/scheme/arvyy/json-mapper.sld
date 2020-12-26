(define-library
  (arvyy json-mapper)
  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (scheme lazy)
          (srfi 180))
  (export
    mapper
    mapper?
    mapper-reader
    mapper-writter
    mapping-error?
    mapping-error-input
    mapping-error-message
    mapping-error-cause
    mapping-error->string
    
    symbol
    number
    str
    boolean
    
    placeholder
    json-node
    nullable-of
    list-of
    vector-of
    alist-of
    record)
  
  (include "json-mapper-impl.scm"))
