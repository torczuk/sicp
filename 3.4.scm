(define (make-account-monitored password balance)
 (let ((invalid-pass 0)
       (invalid-pass-limit 4))
  (define (deposit amount)
   (set! balance (+ balance amount))
   balance)

  (define (withdraw amount)
   (if (> 0 (- balance amount))
    'insufficient-balance
    (set! balance (- balance amount))))

  (define (deffer operation pass amount)
   (cond
    ((eq? pass password)
     (if (eq? operation 'deposit)
      (deposit amount)
      (withdraw amount)))
    (else
     (set! invalid-pass (+ 1 invalid-pass))
     (if (> invalid-pass invalid-pass-limit)
      'call-the-police))))
  deffer))
