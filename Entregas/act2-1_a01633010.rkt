#lang racket

; Francisco Salcedo
; a01633010
; TC-2037


; Dot Product #15

(define (dot-product l1 l2)
  (loop-15 l1 l2 0)
  )

(define (loop-15 n1 n2 sum)
  (if (not(and (empty? n1) (empty? n2)))
      (loop-15 (cdr n1) (cdr n2) (+ sum (* (car n1) (car n2))))
      sum

      ))


; Arithmetic Mean #16
(define (average l)

(loop-14 l 0 0)

)

(define (loop-14 n sum len)

(if (empty? n)
  (/ sum len )
  (loop-14 (cdr n) (+ sum (car n)) (+ 1 len))
)
)


; Std. Deviation #17

(define (standard-deviation l)
  (loop-17 l 0 (average l) 0)
  )

(define (loop-17 n diff-sum avg len)

  (if (empty? n)
      (sqrt (/ diff-sum len) )
      (loop-17 (cdr n) (+ diff-sum (expt (- (car n) avg) 2)) avg (+ len 1))
  )
  )
  

; Binary #20

(define (binary n)
  (loop-20 '() n)

  )

(define (loop-20 ls dec)

  (cond
    [(= dec 0) (reverse ls)] 
    [(= (remainder dec 2) 1) (loop-20 (append ls (list 1)) (floor (/ dec 2)))] 
    [(= (remainder dec 2) 0) (loop-20 (append ls (list 0)) (floor (/ dec 2)))]
    )
  )
