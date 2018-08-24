; one dimentional table function

(define (lookup key table)
 (let ((record (assoc key (cdr table))))
  (if record (cdr record) #f)))

(define (assoc key record)
 (cond ((null? record) #f)
       ((eq? (caar record) key) (car record))
       (else (assoc key (cdr record)))))

(define (insert! key value table)
 (let ((record (assoc key (cdr table))))
  (if record
   (set-cdr! record value)
   (set-cdr! table (cons (cons key value) (cdr table))))))

;; (define t (cons '*table* '()))
;; (insert! 'a 1 t)
;; (insert! 'b 2 t)
;; (lookup 'b t)
