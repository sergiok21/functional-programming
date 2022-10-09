#lang scheme


; ===== Question 1 =====

(define r 0)

; Recursion level (Macros)
(define-syntax rec
  (syntax-rules () ; визначення правила
    ( (_ r) (begin (set! r (+ r 1)) r)) ; інкремент числа
    )
  )

; Square
(define (square b)
  (* b b)) ; добуток числа

; Mod of numbers and show
(define (mod-number? n)
  (= (remainder n 2) 0)) ; визначення умови ділення числа n на 2 із остачею 0 

; Result mod
(define (mod-powing b m)
  (display "b^p = ")(display b) (newline)
  (display "b^p mod m = ") (display (remainder b m)) ; відображення остачі від ділення
  (newline) (display "Recursion level: ") (display r)) ; відображення глибини рекурсії

; Main
(define (exp b n m result)
  (rec r) ; виклик процедури інкременту
  (cond ((= n 1)
        (mod-powing result m)) ; кінець обробки значень -> перевірка остачі від ділення шляхом виклику відповідної процедури
        ( (= n 0) (display "Result: ") 1) ; відображення результату
        ( (mod-number? n) (exp b (/ n 2) m (square result)) ) ; предикат із визначенням остачею (True / False). Далі рекурсія із передачею відповідних значень та параметрів
        ( else (exp b (- n 2) m (* b (* b (sqrt (square result))))) ) ; альтернативне розкриття вказаної формули через корінь
        )
  )


; Run
(display "===== Question 1 =====") (newline)

(display "Enter b: ")
(define b (read)) (newline) ; отримання значення із консолі

(display "Enter p: ")
(define p (read)) (newline)

(display "Enter m: ")
(define m (read)) (newline)

(exp b p m b) ; Виклик процедури


; ===== Question 2 =====

; Main
(define (sub n)
  (cond ((= n 1)
         (display n))
        ( (even? n) (sub (- n 1))) ; предикат перевірки на парність числа -> рекурсія із декриментом значення
        ( else (display n) (newline) (sub (- n 1))) ))


; Run
(newline) (newline)
(display "===== Question 2 =====") (newline)

(sub 8)
