(define (make-queue)
 (let ((front-ptr '())
       (rear-ptr '()))

  (define (empty-queue?)
   (null? front-ptr))

  (define (insert-queue! item)
   (let ((new-pair (cons item '())))
    (cond
     ((empty-queue?)
      (set! front-ptr new-pair)
      (set! rear-ptr new-pair))
     (else
      (set-cdr! rear-ptr new-pair)
      (set! rear-ptr new-pair)))
   ))

  (define (delete-queue!)
   (cond
    ((empty-queue?)
     'invalid-operation)
    (else
     (set! front-ptr (cdr front-ptr)))))

  (define (print-queue)
   display front-ptr)

  (define (dispatch m)
   (cond
    ((eq? m 'delete-queue!)
     delete-queue!)
    ((eq? m 'insert-queue!)
     insert-queue!)
    ((eq? m 'empty-queue?)
     (empty-queue?))
    ((eq? m 'print-queue)
     (print-queue))
    (else
     'invalid-operation)
   ))
  dispatch))

(define (empty-queue? queue)
 ((queue 'empty-queue?)))

(define (insert-queue! queue item)
 ((queue 'insert-queue!) item))

(define (delete-queue! queue)
 ((queue 'delete-queue!)))

(define (print-queue queue)
 (queue 'print-queue))
