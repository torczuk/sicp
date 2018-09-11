(define (make-table equals?)
 (let ((local-table (list '*table*)))

  (define (assoc key record)
   (cond ((null? record) #f)
    ((not (pair? record)) #f)
    ((equals? (caar record) key) (car record))
    (else (assoc key (cdr record)))))

  (define (lookup-t keys table)
   (if (null? keys)
    (cdr table)
   (let ((subtable (assoc (car keys) (cdr table))))
    (if subtable
     (lookup-t (cdr keys) subtable)
     #f))))

  (define (lookup keys)
   (lookup-t keys local-table))

  (define (insert-t! keys value table)
   (if (null? keys)
    (error "Empty keys!")
    (let* ((key (car keys))
          (subtable (assoc key (cdr table))))
     (if (null? (cdr keys))
      (if subtable
       (let ((new subtable))
        (set-cdr! subtable value)
        new)
       (let ((new (cons (cons key value) (cdr table))))
        (set-cdr! table new)
         new))
      (if subtable
       (let ()
        (set-cdr! subtable '())
        (insert-t! (cdr keys) value subtable))
       (let ((new (insert-t! (list key) '() table)))
        (insert-t! (cdr keys) value (car new))))))))

  (define (insert! keys value)
   (insert-t! keys value local-table))

  (define (dispatch m)
   (cond ((eq? m 'lookup-proc) lookup)
    ((eq? m 'insert-proc!) insert!)
    ((eq? m 'show) local-table)
    (else (error "Unknown operation -- TABLE" m))))
  dispatch))

(define operation-table (make-table eq?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define show (operation-table 'show))

;; sample
(put (list 'a 'b) 1)
(get (list 'a 'b))
(put (list 'b) 2)
show
