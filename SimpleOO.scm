#lang scheme

# a detailed explanation of the code can be found on my blog at
# http://learnmeahaskell.blogspot.com/2011/01/nhr-structure-and-interpretation-of.html

(define (make-account balance)
  (let ((set-balance (lambda (x) (set! balance x)))
        (get-balance (lambda () balance)))
     (make-account-internal set-balance get-balance)))

(define (make-account-internal setbal getbal)
  (define (withdraw amount)
     (if (>= (getbal) amount)
         (begin (setbal (- (getbal) amount))
                (getbal))
         "Insufficient Funds"))
  (define (deposit amount)
     (setbal (+ (getbal) amount))
     (getbal))
  (define (dispatch msg)
     (cond ((eq? msg 'withdraw) withdraw)
           ((eq? msg 'deposit) deposit)
           (else (error "Unknown request"
                        msg))))
  dispatch)


(define (make-overdraft-account balance)
  (let ((set-balance (lambda (x) (set! balance x)))
        (get-balance (lambda () balance)))
    (make-overdraft-account-internal set-balance get-balance)))

(define (make-overdraft-account-internal setbal getbal)
  (let  ((parent-dispatch (make-account-internal setbal getbal)))
    (define (withdraw amount)
      (if (and (>= (getbal) 0) (>= (+ (getbal) 100) amount))
          (begin (setbal (- (getbal) amount))
                 (getbal))
          "Insufficient Funds"))
    (define (dispatch msg)
      (cond ((eq? msg 'withdraw) withdraw)
            (else (parent-dispatch msg))))
    dispatch))


(define (make-limited-account balance)
  (let ((set-balance (lambda (x) (set! balance x)))
        (get-balance (lambda () balance)))
     (make-limited-account-internal set-balance get-balance)))

(define (make-limited-account-internal setbal getbal) 
  (let  ((parent-dispatch '())
          (limit 1000))
    (define (deposit amount)
      (if (<= (+ (getbal) amount) limit)
          (begin (setbal (+ (getbal) amount))
                 (getbal))
          "Over Limit"))
    (define (set-proto proto)
      (set! parent-dispatch proto))
    (define (dispatch msg)
      (cond ((eq? msg 'deposit) deposit)
            ((eq? msg 'set-proto) set-proto)
            ((not (null? parent-dispatch)) (parent-dispatch msg))
            (else (error "Unknown request" msg))))
    dispatch))


(define acc (make-account 50))
(define acc3 (make-limited-account 50))





(define acc2 (make-overdraft-account 50))
