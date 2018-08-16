(define (make-deque)

 (define (single-node val)
  (cons val (cons '() '())))

 (define (value node)
  (car node))

 (define (prev node)
  (cadr node))

 (define (next node)
  (cddr node))

 (define (set-next-node! this next-node)
  (set-cdr! (cdr this) next-node))

 (define (set-prev-node! this prev-node)
  (set-car! (cdr this) prev-node))


 (let ((front-ptr '())
       (rear-ptr '()))

  (define (empty-deque?)
   (null? front-ptr))

  (define (front-deque)
   (value front-ptr))

  (define (rear-deque)
   (value rear-ptr))

  (define (rear-insert-deque! item)
   (let ((new-pair (single-node item)))
    (cond
     ((empty-deque?)
      (set! front-ptr new-pair)
      (set! rear-ptr new-pair))
     (else
      (set-next-node! rear-ptr new-pair)
      (set-prev-node! new-pair rear-ptr)
      (set! rear-ptr new-pair)))))

  (define (front-insert-deque! item)
   (let ((new-pair (single-node item)))
    (cond
     ((empty-deque?)
      (set! front-ptr new-pair)
      (set! rear-ptr new-pair))
     (else
      (set-next-node! new-pair front-ptr)
      (set-prev-node! front-ptr new-pair)
      (set! front-ptr new-pair)))))

  (define (front-delete-deque!)
   (cond
     ((empty-deque?)
     'deque-is-empty)
     (else
      (set! front-ptr (next front-ptr))
      (set-prev-node! front-ptr '()))))

  (define (rear-delete-deque!)
   (cond
    ((empty-deque?)
     'invalid-operation)
    (else
     (set! rear-ptr (prev rear-ptr))
     (set-next-node! rear-ptr '()))))

  (define (print-deque)
   (define (print node)
     (cond
       ((null? node) display '())
       (else
        (display (value node))
        (print (next node)))))
   (print front-ptr))

  (define (dispatch m)
   (cond
    ((eq? m 'empty-deque?)
     (empty-deque?))
    ((eq? m 'front-deque)
     (front-deque))
    ((eq? m 'rear-deque)
     (rear-deque))
    ((eq? m 'front-insert-deque!)
     front-insert-deque!)
    ((eq? m 'rear-insert-deque!)
     rear-insert-deque!)
    ((eq? m 'front-delete-deque!)
     (front-delete-deque!))
    ((eq? m 'rear-delete-deque!)
     (rear-delete-deque!))
    ((eq? m 'print-deque)
     (print-deque))
    (else
     'invalid-operation)))
  dispatch))
