(define and-gate-delay 0)
(define invert-delay 0)

(define (logical-and a b)
 (cond ((= a 0) 0)
  ((= b 0) 0)
  (else 1)))

(define (logical-not s)
 (cond ((= s 0) 1)
  ((= s 1) 0)
  (else (error "Invalid signal" s))))

(define (inverter input output)
 (define (invert-input)
  (let ((new-value (logical-not (get-signal input))))
   (after-delay invert-delay
    (lambda () (set-signal! output new-value)))))

 (add-action! input invert-input)
 'ok)

(define (and-gate a1 a2 output)
 (define (and-action-procedure)
  (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
   (after-delay and-gate-delay
    (lambda () (set-signal! output new-value)))))

 (add-action! a1 and-action-procedure)
 (add-action! a2 and-action-procedure)
 'ok)


(define (or-gate a1 a2 output)
 (let ((not-a1 (make-wire))
       (not-a2 (make-wire))
       (not-and (make-wire)))
  (invert a1 not-a1)
  (invert a2 not-a2)
  (not-and not-a1 not-a1 not-and)
  (invert not-a1 output)
  'ok))
