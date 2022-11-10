#lang scheme


(define (numer x) (car x))			;чисельник
(define (denom x) (cdr x))		;знаменник
(define (make-rat n d) (cons n d))		; створення пари
(define(print-rat x)			; друк пари 
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))


(define (mul-rat x y)
          (make-rat (* (numer x) (numer y))
                            (* (denom x) (denom y))))
(display "Question 1: ") (print-rat (mul-rat (make-rat 1 2) (make-rat 3 4))) (newline)


;=====множення та ділення компл чисел в тригонометричній формі======
(define (Myreal-part z) (car z))
(define (Myimag-part z) (cdr z))

(define (square x)
  (* x x))
(define (magnitude1 z)
  (sqrt (+ (square (Myreal-part z)) (square (Myimag-part z)))))  
(define (angle1 z)
  (atan (Myimag-part z) (Myreal-part z)))
(define (make-from-real-imag x y)
  (cons x y))
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a)))
  (display (* r (cos a))) (display " + ") (display (* r (sin a))) (display "i"))
  ;(exact->inexact (/(* r (cos a)) (* r (sin a))))
  ;(remainder (* r (cos a)) (* r (sin a)))

(define (div-complex z1 z2)
        (make-from-mag-ang (/ (magnitude1 z1) (magnitude1 z2))
                           (- (angle1 z1) (angle1 z2))))


(display "Question 2: ")
(newline)
(div-complex (cons 1 10) (cons 1 5))

