#lang scheme

(define min_index '())
(define max_index '())

(define min_value 0)
(define max_value 0)

(define lst '())

(define stack '())
(define stack_previous '())
(define queue '())


; Find min max values with index function
(define (min_max_vector v i)
  (let ((l (vector-length v))) ; Variable L with vector length value
    (if (= i l) (show_vector v)
        (let ((x (vector-ref v i))) ; Get value from V with index I
          (cond
               ( (< x 0)
                 (cond
                   ( (< x min_value) (set! min_value x) (set! min_index i) (min_max_vector v (+ i 1)) ) ; Set value to min_value & min_index
                   (else (min_max_vector v (+ i 1))) ; Recursion
                   )
                 )
               (else
                (cond
                  ( (> x max_value) (set! max_value x) (set! max_index i) (min_max_vector v (+ i 1)) ) ; set value to variables
                  (else (min_max_vector v (+ i 1)))))))))) ; Recursion

; Show information function
(define (show_vector v)
  (display "Vector: ") (display v) (display "; Length: ") (display (vector-length v)) (newline) (newline)
  (cond
    ( (null? min_index) (display "Error for min value!") (newline) ; if min index does not find
                        (display "Max vector value: ") (display max_value) (display "; Index: ") (display max_index) (newline))
    ( (null? max_index) (display "Min vector value: ") (display min_value) (display "; Index: ") (display min_index) (newline)
                        (display "Error for max value!") (newline)) ; if max index does not find
    (else (display "Min vector value: ") (display min_value) (display "; Index: ") (display min_index) (newline)
          (display "Max vector value: ") (display max_value) (display "; Index: ") (display max_index) (newline))))

; Insert value to vector function
(define (insert_value_vector n)
  (let ((l (length lst))) ; Local variable of list length
    (cond
    ( (= n l) (min_max_vector (list->vector lst) 0) ) ; Call function with list->vector
    (else (display "Enter ") (display l) (display " value: ") (set! lst (append lst (list (read)))) (insert_value_vector n) )))) ; Insert value to list

; =========================

; Make queue
(define (make-queue)
 (define p (cons '() '() ) ) ; Make a couple with P
 (cons p p) ; Make a couple
)

; Null queue
(define (null-queue? q)
 (and (eq? (front q) (rear q)) (eq? (car (front q)) '() )) ; Eq first value with next values & first value with empty list
)

; Front
(define (front q)
 (car q)) ; Fist value

; Rear
(define (rear q)
 (cdr q)) ; Next values

; Push
(define (push q e)
 (define p (cons e '()))
 (if (null-queue? q) ; If queue not null
  (begin (set! q p) ; Set value
   (set! q p)
  )
  (begin
   (set! q p)
  ) )
  (set! queue (append queue q))) ; Set queue

; Delete item from stack list
(define (delete_stack item list)
  (set! stack (filter
   (lambda (x) (not (equal? x item))) ; If every elemenet from list does not equal item -> set stack
   list)))

; Delete item from queue list
(define (delete_queue item list)
  (filter
   (lambda (x) (not (equal? x item))) ; --//--
   list))

; Show stack & queue
(define (show-stack-queue)
  (newline)
  (display "Got stack: ") (display stack_previous) (newline)
  (display "Refactored stack: ") (display stack) (newline) (newline)
  (display "Queue: ") (display queue) (newline)
  )

; Insert value to stack function
(define (insert_value_stack n)
  (let ((l (length stack))) ; Local variable of list length
    (cond
    ( (= n l) (line_i (car stack) (cdr stack) )) ; Call two-demensional "cycle"
    (else (display "Enter ") (display l) (display " value: ") (set! stack (append stack (list (read)))) (set! stack_previous (box stack)) (insert_value_stack n) )))) ; Insert value to list

; Two-dimensional "cycle" for find repetition of numbers from stack
(define (line_i i_car i_cdr) ; 1-st cycle - for 1 element
  (define (line_j j_car j_cdr) ; 2-nd cycle - for all elements
    (cond
      ((eq? (length i_cdr) 1) ; If list length == 1
       (cond
         ( (not (eq? i_car j_car)))
         (else (delete_stack i_car stack)))) ; Remove item from stack
       (else
        (cond
          ((null? j_cdr)
           (cond
             ((not (eq? i_car j_car)) )
             (else (line_i (car (cdr i_cdr)) (cdr i_cdr)))))
          (else
           (cond
             ((not (eq? i_car j_car)) (line_j (car j_cdr) (cdr j_cdr)))
             (else (delete_stack i_car stack))))))))
    
  ; Send element for making queue
  (define (send_element l_car l_cdr)
    (cond
      ((null? l_cdr) (push (make-queue) l_car) (set! queue (reverse queue)) (show-stack-queue)) ; If empty cdr -> make a queue & show information
      (else (push (make-queue) l_car) (send_element (car l_cdr) (cdr l_cdr))) ; Recursion with making a queue
      )
    )

  (cond
    ((null? i_cdr)
     (if (and (not (null? stack)) (not (null? (cdr stack)))) (send_element (car stack) (cdr stack)) ; If stack does not null -> call send_element function
         (display "Wrong data of stack..."))) ;(send_element (car stack_new) (cdr stack_new)))
    (else (line_j (car i_cdr) (cdr i_cdr)) (line_i (car i_cdr) (cdr i_cdr)))) ; Call 2-nd "cycle"
  )

; Question 1
(display (make-string 3 #\*)) (display "Question 1") (display (make-string 3 #\*)) (newline) (newline) ; make-string n p, де n (number) - число, p (pattern) - символ.
(display "Enter the length of vector: ") (insert_value_vector (read)) (newline)


; Question 2
(display (make-string 3 #\*)) (display "Question 2") (display (make-string 3 #\*)) (newline) (newline) ; make-string n p, де n (number) - число, p (pattern) - символ.
(display "Enter the length of stack: ") (insert_value_stack (read))

