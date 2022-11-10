#lang scheme


; Початкові значення завдання 1
(define a 0.5)
(define b 2)

; Початкові значення завдання 2
(define a1 1)
(define b1 2)
(define n 1000)
(define h (/ (- b1 a1) n))


;Перша функцiя
(define (f x0)
    (/ (+ (expt (exp 1) x0) (log x0 (exp 1))) 10)
  )


; Друга функція
(define (f1 x0)
    (* x0 (expt (exp 1) (* -1 x0)))
  )


; Похідна
(define (df x0)
    (+ (/ (expt (exp 1) x0) 10) (/ 1 (* 10 x0)))
  )


; Початкове наближення
(define x0 (/ (+ a b) 2))


; Наближення
(define xn (- x0 (/ (f x0) (df x0))))


; Метод дотичних
(define (dot xn i)
  (display "\n") (display (+ i 1)) (display  " ітерація = ") (display (- xn (/ (f xn) (df xn))))
  (display  "; Різниця = ") (display (abs (- xn (- xn (/ (f xn) (df xn))))))
  (if (> (abs (- xn (- xn (/ (f xn) (df xn)))))) (newline)
      (dot (- xn (/ (f xn) (df xn))) (+ i 1))
      
  ))


; Метод ітерацій
(define (iterations x0)
  (display "\nx0 = ") (display x0) (display "\nx1 = ") (display (f x0))
  (cond
    ((real? (f x0)) ; Комплексне число?
     (display "\nНаближення = ") (display (abs (- x0 (f x0))))
     (if (> (abs (- x0 (f x0)))) (newline)
         (iterations (f x0))
         )
     )
    )
)


; Лiвий прямокутник
(define (left_rectangle a h n i sum)
  (cond
    ((<= i (- n 1))
     (left_rectangle a h n (+ i 1) (+ sum (* h (f1 (+ a (* i h))))))
     )
    (else
     sum
     )
    )
  )


;Правий прямокутник
(define (right_rectangle a h n i sum)
  (cond
    ((<= i n)
     (right_rectangle a h n (+ i 1) (+ sum (* h (f1 (+ a (* i h))))))
     )
    (else
     sum
     )
    )
  )


; Сiмпсона
(define (Sympson a h n i sum)
  (cond
    ((<= i (- n 1))
     (Sympson a h n (+ i 1) (+ sum (* (+ 2 (* 2 (remainder (+ i 1) 2))) (f1 (+ a (* i h))))))
     )
    (else
     sum
     )
    )
  )



(display (make-string 3 #\*)) (display "Завдання 1") (display (make-string 3 #\*)) (newline) (newline) ; make-string n p, де n (number) - число, p (pattern) - символ.
(display "Метод ітерацій:\n")
(dot xn 0)
(define check (df x0))
(cond
  ((< check 1) (display "Перевірка наближення: ") (display check) (display " < 1  =>  True") (display "\n\nМетод дотичних:\n") (iterations x0))
  (else (display "\nНаближення: ") (display check) (display " < 1  =>  False"))
  )

(newline) (newline)
(display (make-string 3 #\*)) (display "Завдання 2") (display (make-string 3 #\*)) (newline) ; make-string n p, де n (number) - число, p (pattern) - символ.
(display "\nПравий трикутник: ")
(display (right_rectangle a1 h n 1 0))
(display "\nЛівий трикутник: ")
(display (left_rectangle a1 h n 0 0))
(display "\nСімпсона: ")
(display (/ (Sympson a1 h n 1 (* (+ (f1 a1) (f1 b1)) (/ h 3))) 3000))