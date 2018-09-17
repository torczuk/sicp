(define (interleave s t)
 (if (null-stream? s)
  t
  (cons-stream (stream-car s)
   (interleave t (stream-cdr s)))))

(define (pairs s t)
 (interleave
  (stream-map (lambda (x) (list (stream-car s) x))
   t)
  (pairs (stream-cdr s) (stream-cdr t))))

;; cause there is no delay on invocation (pairs (stream-cdr s) (stream-cdr t))
;; this will cause 'maximum recursion depth exceeded' error
