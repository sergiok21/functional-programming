#lang scheme

(define i 2)
(define l_tangle '(1 6))
(define l_a '())
(define l_b '())
(define l_b2 '())
(define l_c '())

(define l_neighbor '())


; Додання елементів
(define (add_item l_car l_cdr)
  (let ((x (- (+ l_car (* 5 i)) (- i 1)))) ; Локальна змінна із зазначеною формулою
         (cond
           ( (null? l_cdr) ; Якщо пустий список
             (cond
               ( (<= n x) (display "List: ") (display l_tangle) (newline) ) ; Якщо значення останнього елементу >= - виводить інформацію про список
               (else (set! l_tangle (append l_tangle (list x))) (set! i (+ i 1)) (add_item (car l_tangle) (cdr l_tangle))) ; Якщо ні, то додає нові елементи до списку. Рекурсія
               )
             )
           (else (add_item (car l_cdr) (cdr l_cdr)))
           )
         )
  )

; Парні числа
(define (result_a l_car l_cdr)
  (cond
    ( (null? l_cdr)
      (if (= (remainder l_car 2) 0) (set! l_a (append l_a (list l_car))) ; Якщо остача від ділення = 0, то додається елемент до списку
          (display "") ))
    (else
     (cond
       ((= (remainder l_car 2) 0) (set! l_a (append l_a (list l_car))) (result_a (car l_cdr) (cdr l_cdr))) ; ---//---
       (else (result_a (car l_cdr) (cdr l_cdr)))
       )
     )
    )
  )

; Кратне 5
(define (multiple l_car l_cdr)
  (cond
    ( (null? l_cdr)
      (if (= (remainder l_car 5) 0) (set! l_b (append l_b (list l_car))) ; ---//---
          (display "") ))
    (else
     (cond
       ( (= (remainder l_car 5) 0) (set! l_b (append l_b (list l_car))) (multiple (car l_cdr) (cdr l_cdr))) ; ---//---
       (else (multiple (car l_cdr) (cdr l_cdr)))
       )
     )
    )
  )

; Добуток на 10
(define (multiplication l_car l_cdr)
  (cond
    ( (null? l_cdr) (set! l_b2 (append l_b2 (list (* l_car 10)))) )
    (else (set! l_b2 (append l_b2 (list (* l_car 10)))) (multiplication (car l_cdr) (cdr l_cdr))) ; Множення на 10
    )
  )

; Кратні 10
(define (result_с l_car l_cdr)
  (cond
    ( (null? l_cdr)
      (if (= (remainder l_car 10) 0) (set! l_c (append l_c (list l_car)))
          (display "") ))
    (else
     (cond
       ((= (remainder l_car 10) 0) (set! l_c (append l_c (list l_car))) (result_с (car l_cdr) (cdr l_cdr)))
       (else (result_с (car l_cdr) (cdr l_cdr)))
       )
     )
    )
  )

; Company schema
(define (add product company_req wait opt_req)
  (cond
    ( (= (length l_neighbor) 2) (display "Neighbor: ") (display (car l_neighbor)) (display ", ") (display (car (cdr l_neighbor))) (newline)
                                (display "(") (display (car (cdr l_neighbor))) (display " x ") (display product) (display ") / ") (display (car l_neighbor))
                                (display " = ") (display (- (/ (* (car (cdr l_neighbor)) product) 12) product)) (display " products / day") )
    ( (= (remainder opt_req (+ company_req wait)) 0) (set! l_neighbor (append l_neighbor (list opt_req))) (add product company_req wait (+ 1 opt_req)))
      (else
       (cond
         ((= (length l_neighbor) 0) (add product company_req wait (- opt_req 1)))
         (else (add product company_req wait (+ 1 opt_req)))
         )
       )
      )
  )

(display (make-string 3 #\*)) (display "Question 1") (display (make-string 3 #\*)) (newline) (newline) ; make-string n p, де n (number) - число, p (pattern) - символ.

(display "Enter n: ") (define n (read))

(add_item (car l_tangle) (cdr l_tangle))
(result_a (car l_tangle) (cdr l_tangle))
(multiple (car l_tangle) (cdr l_tangle) )
(multiplication (car l_b) (cdr l_b))
(result_с (car l_tangle) (cdr l_tangle))
(set! l_tangle (append l_tangle (list l_c))) ; Створення підсписку

(display "\nA:\n") (display l_a) (display " => ") (display (apply + l_a))
(display "\nB:\n") (display l_b2)
(display "\nC:\n") (display l_tangle)

(newline) (newline)

(display (make-string 3 #\*)) (display "Question 2") (display (make-string 3 #\*)) (newline) (newline) ; make-string n p, де n (number) - число, p (pattern) - символ.

(add 60 5 1 14)
