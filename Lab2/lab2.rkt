#lang scheme

; ========== Question 1 ==========

(define x-start -1.5) ; Значення початкового значення x.
(define x-end 2) ; Значення кінцевого значення x.
(define y0 1) ; Початкове значення y0.
(define avg 0) ; Змінна для середнього значення.
(define avg-abs 0) ; Змінна для середнього значення абсолюта.
(define delta-y 0) ; Змінна для відносної похибки.

(define list-values '()) ; Список для значень.
(define list-abs-values '()) ; -//-

; Square
(define (square x)
  (* x x))

; Display value
(define (show x yn)
  (display "X:") (newline) (display (- x .5)) (newline) (newline) (display "Y:") (newline) (display yn) (newline) (display (make-string 20 #\=)) (newline)
  )

; Absolute error
(define (abs-error el lsts)
  (cond
    ( (null? (cdr lsts)) (set! list-abs-values (append list-abs-values (list (- el avg)))) ; "Передача" значень до списку, шляхом додання значень до списку, де відбувається різниця значень.
                         (set! list-abs-values (append list-abs-values (list (- (car lsts) avg)))) ; "Передача" значень до списку, шляхом додання значень до списку.
                         (set! avg-abs (string->number(substring(number->string (/ (apply + list-abs-values) (length list-abs-values))) 0 4))) ; "Передача" значення із наближеним значенням, де:
                                                                                                                                               ; 1) Сума значень списка.
                                                                                                                                               ; 2) Ділення на довжину списка.
                                                                                                                                               ; 3) Конвертація числа у тип string.
                                                                                                                                               ; 4) Виріз значень від 0 до 4.
                                                                                                                                               ; 5) String -> number.
                                                                                                                                               ; 6) Призначення значення до змінної.
                         (set! delta-y (- avg avg-abs)))
    ( else (set! list-abs-values(append list-abs-values (list (- el avg)))) (abs-error (car lsts) (cdr lsts)) ) ; Додання різниці значень до списку, й виклик ф-ції із передачею параматреів першого та усіх останніх ел-тів списку.
    )
  )

; Main
(define (main x yn)
  (cond ((<= x x-end)
         (show x yn)
       (cond
         ( (and (<= 1 x) (<= x 2)) (set! list-values (append list-values (list yn))) (main (+ x .5) (* (/ 1 2) (/ (+ yn (- 15 (square x))) yn )) ) ) ; Розкриття формули у ряд Тейлора із дотриманням умов.
         ( (and (<= -1 x) (< x 1)) (set! list-values (append list-values (list yn))) (main (+ x .5) (/ 1 (* (/ 1 2) (/ (+ yn (+ x (square x))) yn )) ) ) ) ; Розкриття формули у ряд Тейлора із дотриманням умов.
         ( else (main (+ x .5) yn) )
         )
       )
      (else
       (show x yn)
       (set! list-values (cdr(append list-values (list yn))))
        )
      )
  )



; Run
(display (make-string 3 #\*)) (display "Question 1") (display (make-string 3 #\*)) (newline) (newline) ; make-string n p, де n (number) - число, p (pattern) - символ.

(main x-start y0)

(display "All values:") (newline) (display list-values) (newline)

(display "AVG y:") (newline) (set! avg (/ (apply + list-values) (length list-values))) (display avg) (newline) (newline) ; Обчислення середнього значення

(abs-error (car list-values) (cdr list-values)) ; Виклик процедури із передачею параметрів першого та останніх елементів

(display "AVG y-abs:") (newline) (display avg-abs) (newline) (newline)
(display "Δy: ") (newline) (display delta-y) (newline) (newline)
(display "Δy / y:") (newline) (display (/ delta-y avg)) (display " -> ") (display (* (/ delta-y avg-abs) 100)) (display "%") (newline) (newline) ; Відносна похибка
(display (make-string 20 #\=)) (newline) (newline)



; ========== Question 2 ==========


; Factorial
(define (factorial n)
  (cond
    ( (= n 0) * 1 3 )
      (else (* n (factorial (- n 1))))
      )
  )


; Run
(display (make-string 3 #\*)) (display "Question 2") (display (make-string 3 #\*)) (newline) (newline)

;(display "Enter n: ") (define n-members (read)) (newline)
(display "Result: ")

(factorial 3)
