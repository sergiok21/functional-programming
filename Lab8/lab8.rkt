#lang scheme


(define lst '())
(define lst_num '())
(define lst_denom '())
(define lst_div '())
(define temp_div "")
(define temp_mul "")
(define temp_num "")
(define check_mul #f)
(define bool_div #f)
(define result "")


(define (empty)
  (set! result (string-append result " "))
  (set! temp_mul ""))

(define (sin_x)
  (set! result (string-append result "cosx")) (empty))

(define (cos_x)
  (set! result (string-append result "-sinx")) (empty))

(define (num x)
  (set! result (string-append result "0")))

(define (just_x x)
  (set! result (string-append result "1")) (empty))

(define (num_x x)
  (set! result (string-append result (substring x 0 (- (string-length x) 1)))) (empty))

(define (sum x)
  (set! result (string-append result x)) (empty))

(define (diff x)
  (set! result (string-append result x)) (empty))

(define (mul x)
  (let ((x1 (string->number (substring temp_num 0 1)))); (- (string-length temp_num) 1)))))
    (let ((x2 (string->number (substring x 0 1)))) ; (- (string-length x) 1)))))
      (set! temp_mul (string-append temp_mul (number->string (* x1 x2))))
      ;(set! temp_mul (string-append temp_mul "x"))
      (set! temp_mul (string-append temp_mul "x^2"))
      ;(display "Step 1: ") (display temp_mul) (newline) (display "Final: ")
      (set! check_mul #f)
      (pow temp_mul)
      )
    )
  )

(define (numerator l_car l_cdr)
  (cond
    ((not (equal? (string-length l_car) 1))
     (cond
       ((null? l_cdr) (set! lst_num (append lst_num (list l_car))) (denominator l_car))
       (else (set! lst_num (append lst_num (list l_car))) (numerator (car l_cdr) (cdr l_cdr)) )
       )
     )
    ((equal? l_car "/") (denominator (car l_cdr)))
    (else (set! lst_num (append lst_num (list l_car))) (numerator (car l_cdr) (cdr l_cdr)) )
    )
  )

(define (denominator x)
  (set! lst_denom (append lst_denom (list (substring x 1 (- (string-length x) 1)))))
  (div (substring (car lst_num) 1 (string-length (car lst_num))) (cdr lst_num) (substring (car lst_denom) 0 1)) ;(substring (car lst_denom) 1 (- (string-length (car lst_denom)) 1)))
  )

(define (check_div? x v)
  (= (remainder x v) 0))

(define (div car_x cdr_x v)
  (let ((x (string->number (substring car_x (- (string-length car_x) 1) (string-length car_x)))))
    (cond
      ((null? cdr_x) (set! lst_div (append lst_div (list (substring car_x 0 (- (string-length car_x) 1))))) (set! bool_div #f) (condition (car lst_div) (cdr lst_div) ) ) ; check value + call func
      (else
       (cond
         ((equal? (string-length car_x) 1) (set! lst_div (append lst_div (list car_x))) (div (car cdr_x) (cdr cdr_x) v) )
         ((equal? x ")")
          (set! lst_div (append lst_div (list (substring car_x 0 (- (string-length car_x) 1)))))
          (condition (car lst_div) (cdr lst_div))
          )
         ((not (equal? x "x"))
          (cond
            ((equal? (check_div? (string->number (substring car_x 0 (- (string-length car_x) 3))) (string->number v)) #t)
             (set! temp_div (string-append temp_div (number->string (/ (string->number (substring car_x 0 (- (string-length car_x) 3))) (string->number v)))))
             (set! temp_div (string-append temp_div "x^"))
             (set! temp_div (string-append temp_div (number->string (- x 1))))
             (set! lst_div (append lst_div (list temp_div)))
             (set! temp_div "")
             (div (car cdr_x) (cdr cdr_x) v))
            (else (set! lst_div (append lst_div (list car_x))) (div (car cdr_x) (cdr cdr_x) v))
            )
          )
         )
       )
      )
    )
  )

(define (pow str)
  (let ((x (string->number (substring str 0 (- (string-length str) 3)))))
       (let ((p (string->number(substring str (- (string-length str) 1) (string-length str)))))
         (cond
         ((= p 1) (set! result (string-append result (number->string x))) (empty))
         (else (set! result (string-append result (number->string (* x p))))
               (set! result (string-append result "x^"))
               (set! result (string-append result (number->string (- p 1))))
               (empty) )))))

(define (condition l_car l_cdr)
  (cond
    ((equal? check_mul #t) (calculation l_car l_cdr))
    ((not (null? l_cdr))
     (cond
       ((not (null? (cdr l_cdr)))
        (cond
          ((equal? (substring l_car 0 1) "(") (set! bool_div #t) (numerator (car lst) (cdr lst)))
          ((equal? (car l_cdr) "*") (set! check_mul #t) (set! temp_num l_car) (condition (car (cdr l_cdr)) (cdr (cdr l_cdr))))
          (else (calculation l_car l_cdr))
        ))
       (else (calculation l_car l_cdr))
       ))
     (else (calculation l_car '()))
    )
  )

(define (calculation l_car l_cdr)
  (cond
    ((equal? check_mul #t) (set! check_mul #f) (mul l_car))
    (else
     (let ((x (substring l_car (- (string-length l_car) 1) (string-length l_car) )))
    (cond
    ((null? l_cdr)
     (cond
           ( (= 1 (string-length l_car))
      (cond
        ((equal? "x" x) (just_x l_car))
        ((equal? "+" l_car) (sum l_car))
        ((equal? "-" l_car) (diff l_car))
        (else (num l_car)) ; Number
        )
      )
    (else
       (cond
        ((equal? "x" x) (num_x l_car))
        ((and (not (equal? "x" x)) ; Sample x
              (equal? "^" (substring l_car (- (string-length l_car) 2) (- (string-length l_car) 1)))) (pow l_car) ) ; Pow
        ((equal? "cos" l_car) (cos_x))
        ((equal? "sin" l_car) (sin_x))
        )
       )
       )
     )
    ( (= 1 (string-length l_car))
      (cond
        ((equal? "x" x) (just_x l_car))
        ((equal? "+" l_car) (sum l_car))
        ((equal? "-" l_car) (diff l_car))
        (else (num l_car)) ; Number
        )
      )
    (else
       (cond
        ((equal? "x" x) (num_x l_car))
        ((and (not (equal? "x" x)) ; Sample x
              (equal? "^" (substring l_car (- (string-length l_car) 2) (- (string-length l_car) 1)))) (pow l_car) ) ; Pow
        ((equal? "cos" l_car) (cos_x))
        ((equal? "sin" l_car) (sin_x))
        )
       )
      )
     ))
    )

  (cond
    ((null? l_cdr) (display result))
    (else (condition (car l_cdr) (cdr l_cdr)))
    )
  )

(define (string-split char-delimiter? string)
  (define (maybe-add a b parts)
    (if (= a b) parts (cons (substring string a b) parts)))
  (let ((n (string-length string)))
    (let loop ((a 0) (b 0) (parts '()))
      (if (< b n)
          (if (not (char-delimiter? (string-ref string b)))
              (loop a (+ b 1) parts)
              (loop (+ b 1) (+ b 1) (maybe-add a b parts)))
          (set! lst (reverse (maybe-add a b parts)))))))


(define s (read-line))
(string-split char-whitespace? s)

(condition (car lst) (cdr lst))

