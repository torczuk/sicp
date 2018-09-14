(define (add-streams s1 s2)
 (stream-map + s1 s2))

(define (stream-map proc . argstreams)
 (if (stream-null? (car argstreams))
  the-empty-stream
  (cons-stream
   (apply proc (map stream-car argstreams))
   (apply stream-map
    (cons proc (map stream-cdr argstreams))))))

(define s (cons-stream 1 (add-streams s s)))

;; this is display power or 2: 1, 2, 4, 8
