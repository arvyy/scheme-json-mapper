(import (srfi 64)
        (scheme base)
        (arvyy json-mapper))

;;rec type for testing
(define-record-type 
  <tree>
  (make-tree left right value)
  tree?
  (left tree-left)
  (right tree-right)
  (value tree-value))

(define (tree-branch-equal? b1 b2)
  (or (and (not b1) (not b2))
      (and (tree? b1) (tree? b2) (tree-equal? b1 b2))))

(define (tree-equal? t1 t2)
  (and (equal? (tree-value t1) (tree-value t2))
       (tree-branch-equal? (tree-left t1)
                           (tree-left t2))
       (tree-branch-equal? (tree-right t1)
                           (tree-right t2))))

(test-begin "Json mapper")

;; on test end exit with non-zero status if there were failures
(let* ((runner (test-runner-current))
       (callback (test-runner-on-final runner)))
  (test-runner-on-final!
    runner
    (lambda (r)
      (callback r)
      (exit (= 0 (test-runner-fail-count r))))))

(test-group 
  "Test integer"
  (test-equal 1 ((mapper-reader number) 1)))

(test-group
  "Test list"
  (test-equal '(1 2 3) ((mapper-reader (list-of number)) #(1 2 3))))

(test-group
  "Test record"
  (define-values
    (tree-json-type tree-mapper-setter!)
    (placeholder))
  (define tree-json-type* 
    (record make-tree
            ('left tree-left (nullable-of tree-json-type) #f)
            ('right tree-right (nullable-of tree-json-type) #f)
            ('value tree-value json-node)))
  (tree-mapper-setter! tree-json-type*)
  (test-assert
    (tree-equal?
      (make-tree
        (make-tree #f #f 1)
        (make-tree #f #f 2) 
        3)
      ((mapper-reader tree-json-type) '((left . ((value . 1))) (value . 3) (right . ((value . 2))))))))

(test-end)
