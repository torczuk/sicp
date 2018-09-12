(define (make-time-segment time queue)
 (cons time queue))

(define (segment-time s)
 (car s))

(define (segment-queue s)
 (cdr s))

(define (make-agenda)
 (list 0))

(define (current-time agenda)
 (car agenda))

(define (set-current-time! agenda time)
 (set-car! agenda time))

(define (segments agenda)
 (cdr agenda))

(define (set-segments! agenda segments)
 (set-cdr! agenda segments))

(define (first-segment agenda)
 (car (segments agenda)))

(define (rest-segments agenda)
 (cdr (segments agenda)))

(define (empty-agenda? agenda)
 (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
 (define (belongs-before? segments)
  (or (null? segments)
   (< time (segment-time (car segments)))))

 (define (make-new-time-segment time action)
  (let ((q (make-queue)))
   (insert-queue! q action)
   (make-time-segment time q)))

 (define (add-to-segments! segments)
  (if (= time (segment-time (car segments)) time)
   (insert-queue! (segment-queue (car segments)) action)
   (let ((rest (cdr segments)))
    (if (belongs-before? rest)
     (set-cdr!
      segments
      (cons (make-new-time-segment time action) rest))
     (add-to-segments! rest)))))

 (let ((segments (segments agenda)))
  (if (belongs-before? segments)
   (set-segments!
    agenda
    (cons (make-new-time-segment time action) segments))
   (add-to-segments! segments)))
)
