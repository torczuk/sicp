(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
 (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (partial-sums s)
 (cons-stream (car s) (add-streams (partial-sums s) (stream-cdr s))))
