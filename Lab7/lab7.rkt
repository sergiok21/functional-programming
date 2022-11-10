#lang scheme

(define _str "")
(define result_str "")
(define temp_counter 0)
(define counter 0)
 
(define lst '())
(define lst_counter '())


(define source
  (open-input-file "E:/data.txt")
  )

(define source_write
  (cond
    ((file-exists? "E:/output.txt") (delete-file "E:/output.txt") (open-output-file "E:/output.txt"))
    (else (open-output-file "E:/output.txt"))
    )
  )

(define (vowel  char)
  (cond ((equal? char "e") (set! temp_counter (+ temp_counter 1)))
        ((equal? char "y") (set! temp_counter (+ temp_counter 1)))
        ((equal? char "u") (set! temp_counter (+ temp_counter 1)))
        ((equal? char "i") (set! temp_counter (+ temp_counter 1)))
        ((equal? char "o") (set! temp_counter (+ temp_counter 1)))
        ((equal? char "a") (set! temp_counter (+ temp_counter 1)))
        (else #f)
        )
  )

(define (change_vowel  char)
  (cond ((equal? char "e") (set! temp_counter (+ temp_counter 1)))
        ((equal? char "y") (set! temp_counter (+ temp_counter 1)))
        ((equal? char "u") (set! temp_counter (+ temp_counter 1)))
        ((equal? char "i") (set! temp_counter (+ temp_counter 1)))
        ((equal? char "o") (set! temp_counter (+ temp_counter 1)))
        ((equal? char "a") (set! temp_counter (+ temp_counter 1)))
        (else #f)
        )
  )

(define (add)
  (let ((line (read-line source)))
     (cond
       ( (eof-object? line) (close-input-port source) (changer 0 (string-length _str)))
       (else (every_args 0 (string-length (string-replace line "\r" "")) (string-replace line "\r" "")) (set! temp_counter 0) (add)) ; (vowel (substring line 0 1))
      )
     )
   )


(define (every_args i len str)
  (cond
    ((<= len i)
     (cond
       ((= counter 0) (set! counter temp_counter) (set! _str str) )
       (else
        (cond
       ((>= counter temp_counter)) ; (every_args (+ 1 i) len str))
       ((< counter temp_counter) (set! counter temp_counter) (set! _str str)) ; (display len) (set! counter temp_counter) (set! _str str) (every_args (+ 1 i) len str))
       ( else (vowel (substring str i (+ (- len len) (+ i 1)))))) 
     )))
    ( else (vowel (substring str i (+ (- len len) (+ i 1)))) (every_args (+ 1 i) len str))))

(define (changer i len)
  (cond
    ((<= len i) (display "Got sentence: ") (display _str) (newline)
                (display "Vowels counts: ") (display counter) (newline)
                (display "Length: ") (display len) (newline) (newline)
                (display "Replaced string: ") (display result_str)
                (display result_str source_write) (close-output-port source_write)
                )
    (else (set! result_str (string-append result_str (number->string i))) (changer (+ i 1) len))
    )
  )


(add)
