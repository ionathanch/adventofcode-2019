#lang racket

(require "../lib.rkt")

(define (string->numbers str)
  (map (λ (c) (- (char->integer c) (char->integer #\0)))
       (string->list str)))

(define input-string (car (problem-input 16)))

(define input
  (string->numbers input-string))

(define input-long
  (apply append (make-list 10000 input)))

(define message-offset
  (string->number (substring input-string 0 7)))

(define (base len n)
  (let* ([base-n (apply append (map (∂ make-list n) '(0 1 0 -1)))]
         [repeats (add1 (ceiling (/ len (* n 4))))]
         [repeated (apply append (make-list repeats base-n))])
    (take (cdr repeated) len)))

(define (prod-sum l1 l2)
  (sum (map * l1 l2)))

(define (fft ns)
  (map (λ (n) (remainder (abs (prod-sum ns (base (length ns) n))) 10))
       (range 1 (add1 (length ns)))))

(define (part1)
  (let loop ([signal input]
             [count 0])
    (if (= 100 count)
        (take signal 8)
        (loop (fft signal) (add1 count)))))