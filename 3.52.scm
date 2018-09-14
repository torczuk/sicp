(define (stream-ref s n)
 (if (= n 0)
  (stream-car s)
  (stream-ref (stream-cdr s) (- n 1))))

(define (display-stream s)
 (stream-for-each display-line s))

(define (stream-enumerate-interval low high)
 (if (> low high)
  the-empty-stream
  (cons-stream
   low
   (stream-enumerate-interval (+ low 1) high))))

(define (display-line x)
 (newline)
 (display x))

(define sum 0)

(define (accum x)
 (set! sum (+ x sum))
 sum)

;;20 sums below 1, 3, 6, 10 ...
(define seq (stream-map accum (stream-enumerate-interval 1 20)))

;; 1,3,15...
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
(stream-ref y 7)
(display-stream z)

;; using (lambda () exp) instead of delay would cause that display-stream z would again calculate sum for
