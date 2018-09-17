(define (interleave s t)
 (if (null-stream? s)
  t
  (cons-stream (stream-car s)
   (interleave t (stream-cdr s)))))

(define (pair s t)
 (cons-stream
  (list (stream-car s) (stream-car t))
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
    (stream-cdr t))
   (pairs (stream-cdr s) (stream-cdr t)))))


(pairs integer integer)

;; number of pairs before (n, m) pair
;; (1 + (m - 1) / 2) * (m  - 1) + (n - 1)
