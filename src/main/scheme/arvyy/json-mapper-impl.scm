(define-record-type 
  <mapper>
  (mapper reader writter)
  mapper?
  (reader mapper-reader)
  (writter mapper-writter))

(define-record-type
  <mapping-error>
  (mapping-error message input cause)
  mapping-error?
  (message mapping-error-message)
  (input mapping-error-input)
  (cause mapping-error-cause))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PRIMITIVES

;;SYMBOL
(define symbol
  (mapper
    (lambda (json)
      (unless (string? json)
        (raise (mapping-error "Symbol reader: expected string" json #f)))
      (string->symbol json))
    (lambda (value)
      (unless (symbol? value)
        (raise (mapping-error "Symbol writter: expected symbol" value #f)))
      (symbol->string value))))

;;STRING
(define str
  (mapper
    (lambda (json)
      (unless (string? json)
        (raise (mapping-error "String reader: expected string" json #f)))
      json)
    (lambda (value)
      (unless (string? value)
        (raise (mapping-error "String writter: expected string" value #f)))
      value)))

;;NUMBER
(define number
  (mapper
    (lambda (json)
      (unless (number? json)
        (raise (mapping-error "Number reader: expected number" json #f)))
      json)
    (lambda (value)
      (unless (number? value)
        (raise (mapping-error "Number writter: expected number" value #f)))
      value)))

;;BOOLEAN
(define boolean
  (mapper
    (lambda (json)
      (unless (boolean? json)
        (raise (mapping-error "Boolean reader: expected boolean" json #f)))
      json)
    (lambda (value)
      (unless (number? value)
        (raise (mapping-error "Boolean writter: expected boolean" value #f)))
      value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MISC

;;PLACEHOLDER
(define (placeholder)
  (define reader #f)
  (define writter #f)
  (define m (mapper
              (lambda (json)
                (unless reader
                  (raise (mapping-error "Placeholder reader: uninitialized" json #f)))
                (reader json))
              (lambda (value)
                (unless writter
                  (raise (mapping-error "Placeholder writter: uninitialized" value #f)))
                (writter value))))
  (define (setter! type)
    (set! reader (mapper-reader type))
    (set! writter (mapper-writter type)))
  
  (values m setter!))

;;JSON-NODE
(define json-node
  (mapper
    (lambda (json) json)
    (lambda (value) value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; COMPOSITES

;;NULLABLE
(define nullable-of
  (case-lambda
    ((type) 
     (nullable-of type 'null #f))
    
    ((type json-null value-null) 
     (nullable-of type 
                  (lambda (json) (equal? json json-null))
                  json-null
                  (lambda (value) (equal? value value-null))
                  value-null))
    
    ((type json-null-pred 
           json-null
           value-null-pred
           value-null)
     (let ((reader (mapper-reader type))
           (writter (mapper-writter type)))
      (mapper
       (lambda (json)
         (if (json-null-pred json)
             value-null
             (reader json)))
       (lambda (value)
         (if (value-null-pred value)
             json-null
             (writter value))))))))

;;VECTOR
(define (vector-of/reader type)
  (define reader (mapper-reader type))
  (lambda (input)
    (define output (let ()
                    (unless (vector? input)
                      (raise (mapping-error "Vector reader: expected vector" input #f)))
                    (make-vector (vector-length input))))
    (let loop ((index 0))
     (if (>= index (vector-length input))
         output
         (begin
           (with-exception-handler
             (lambda (err)
               (when (mapping-error? err)
                 (raise (mapping-error (string-append "Vector reader: failed reading element at index "
                                                      (number->string index))
                                       input
                                       err))))
             (lambda ()
               (vector-set! output index (reader (vector-ref input index)))))
           (loop (+ 1 index)))))))

(define (vector-of/writter type)
  (define writter (mapper-writter type))
  (lambda (input)
    (define output (let ()
                    (unless (vector? input)
                      (raise (mapping-error "Vector writter: expected vector" input #f)))
                    (make-vector (vector-length input))))
    (let loop ((index 0))
     (if (>= index (vector-length input))
         output
         (begin
           (with-exception-handler
             (lambda (err)
               (when (mapping-error? err)
                 (raise (mapping-error (string-append "Vector writter: failed writting element at index "
                                                      (number->string index))
                                       input
                                       err))))
             (lambda ()
               (vector-set! output index (writter (vector-ref input index)))))
           (loop (+ 1 index)))))))

(define (vector-of type)
  (mapper (vector-of/reader type)
          (vector-of/writter type)))

;;LIST
(define (list-of/reader type)
  (define reader (mapper-reader type))
  (lambda (input)
    (define output (let ()
                    (unless (vector? input)
                      (raise (mapping-error "List reader: expected vector" input #f)))
                    (make-vector (vector-length input))))
    (let loop ((index 0))
     (if (>= index (vector-length input))
         (vector->list output)
         (begin
           (with-exception-handler
             (lambda (err)
               (when (mapping-error? err)
                 (raise (mapping-error (string-append "List reader: failed reading element at index "
                                                      (number->string index))
                                       input
                                       err))))
             (lambda ()
               (vector-set! output index (reader (vector-ref input index)))))
           (loop (+ 1 index)))))))

(define (list-of/writter type)
  (define writter (mapper-writter type))
  (lambda (input)
    (unless (list? input)
      (raise (mapping-error "List writter: expected list" input #f)))
    (let loop ((index 0)
               (input input)
               (output '()))
      (cond
        ((null? input) (reverse output))
        (else (let ()
                (define entry 
                  (with-exception-handler
                    (lambda (err)
                      (when (mapping-error? err)
                        (raise (mapping-error (string-append "List writter: failed writting element at index "
                                                             (number->string index))
                                              input
                                              err))))
                    (lambda ()
                      (writter (car input)))))
                (loop (+ 1 index)
                      (cdr input)
                      (cons entry output))))))))

(define (list-of type)
  (mapper (list-of/reader type)
          (list-of/writter type)))

;;ALIST
(define (alist-of/reader type)
  (define reader (mapper-reader type))
  (lambda (json)
    (unless (and (list? json))
      (raise (mapping-error "Alist reader: expected alist" json #f)))
    (map
      (lambda (entry)
        (define key (car entry))
        (define value 
          (with-exception-handler
            (lambda (err)
              (when (mapping-error? err)
                (raise (mapping-error (string-append "Alist reader: failed reading element with key "
                                                     (symbol->string key))
                                      (cdr entry)
                                      err))))
            (lambda ()
              (reader (cdr entry)))))
        (cons key value))
      json)))

(define (alist-of/writter type)
  (define writter (mapper-writter type))
  (lambda (json)
    (unless (and (list? json))
      (raise (mapping-error "Alist writter: expected alist" json #f)))
    (map
      (lambda (entry)
        (define key (car entry))
        (define value 
          (with-exception-handler
            (lambda (err)
              (when (mapping-error? err)
                (raise (mapping-error (string-append "Alist writter: failed reading element with key "
                                                     (symbol->string key))
                                      (cdr entry)
                                      err))))
            (lambda ()
              (writter (cdr entry)))))
        (cons key value))
      json)))

(define (alist-of type)
  (mapper (alist-of/reader type)
          (alist-of/writter type)))

;;RECORD
(define-syntax record
  (syntax-rules ()
    ((_ constructor fields ...)
     (record-pr constructor () (fields ...)))))

(define (record-field/reader field type default-promise)
  (define reader (mapper-reader type))
  (lambda (json)
    (cond
      ((assoc field json) => (lambda (entry)
                               (with-exception-handler
                                 (lambda (err)
                                   (when (mapping-error? err)
                                     (raise (mapping-error (string-append "Record reader: failed reading field's "
                                                                          (symbol->string field)
                                                                          "value")
                                                           (cdr entry)
                                                           err))))
                                 (lambda ()
                                   (reader (cdr entry))))))
      (else (force default-promise)))))

(define (record-field/writter field getter type)
  (define writter (mapper-writter type))
  (lambda (value)
    (define field-value (getter value))
    (define output
      (with-exception-handler
        (lambda (err)
          (when (mapping-error? err)
            (raise (mapping-error (string-append "Record writter: failed writting fields's "
                                                 (symbol->string field)
                                                 "value")
                                  field-value
                                  err))))
        (lambda ()
          (writter field-value))))
    (cons field output)))

(define-syntax record-pr
  (syntax-rules ()
    ((_ constructor (parsed ...) ((field getter type) rest ...))
     (record-pr constructor (parsed ...  (field getter type (raise (mapping-error (string-append 
                                                                                    "Record reader: field "
                                                                                    (symbol->string field)
                                                                                    " not found, and no default value provided")
                                                                                  #f
                                                                                  #f)))) (rest ...)))
    
    ((_ constructor (parsed ...) ((field getter type default) rest ...))
     (record-pr constructor (parsed ... (field getter type default)) (rest ...)))
    
    ((_ constructor ((field getter type default) ...) ())
     (let ((readers (list (record-field/reader field type (delay default)) ...))
           (writters (list (record-field/writter field getter type) ...)))
       (mapper
         (lambda (json)
           (define args (map (lambda (reader) (reader json)) readers))
           (apply constructor args))
         (lambda (value)
           (map
             (lambda (writter)
               (writter value))
             writters)))))))
