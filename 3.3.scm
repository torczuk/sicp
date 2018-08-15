(define (make-account-secured password balance)

 (define (deposit amount)
  (set! balance (+ balance amount))
  balance)

 (define (withdraw amount)
  (if (> 0 (- balance amount))
   'insufficient-balance
   (set! balance (- balance amount))))

 (define (deffer operation pass amount)
  (if
   (eq? pass password)
   (if (eq? operation 'deposit)
    (deposit amount)
    (withdraw amount))
   'invalid-password))
 deffer)
