(define (interleave s t)
 (if (null-stream? s)
  t
  (cons-stream (stream-car s)
   (interleave t (stream-cdr s)))))

(define (pair s t)
 (cons-stream
  (list (stream-car s) ((stream-car t)))
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
    (stream-cdr t))
   (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
 (cons-stream
  (list (stream-car s) (stream-car t) (stream-car u))
  (interleave (stream-map (lambda (pair) (cons (stream-car s) pair)) (stream-cdr (pair s t)))
              (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (sqrt n)
 (* n n))

(define pythagorean (
 (define (is-pythagorean? triple)
  (let ((a (car triple))
        (b (cadr triple))
        (c (caddr triple)))
  (= (+ (sqrt a) (sqrt b)) (sqrt c))))

 (stream-filter is-pythagorean? (triples integers integers integers))))
