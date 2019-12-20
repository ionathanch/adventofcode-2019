#lang racket

(require "../lib.rkt")

(define (string->numbers str)
  (map (λ (c) (- (char->integer c) (char->integer #\0)))
       (string->list str)))

(define input-string (car (problem-input 16)))

(define input
  (string->numbers input-string))

(define (base n)
  (let* ([bs (apply append (map (∂ make-list n) '(0 1 0 -1)))])
    (rac (cdr bs) (car bs))))

(define (prod-sum l1 l2)
  (let* ([len (min (length l1) (length l2))]
         [l1 (take l1 len)]
         [l2 (take l2 len)])
    (sum (map * l1 l2))))

(define (fft ns)
  (let* ([base-reps (λ (n) (ceiling (/ (length ns) (* 4 n))))]
         [base-n (λ (n) (repeat (base-reps n) (base n)))])
    (map (λ (n) (remainder (abs (prod-sum ns (base-n n))) 10))
         (range 1 (add1 (length ns))))))

(define (part1)
  (let loop ([signal input]
             [count 0])
    (if (= 100 count)
        (take signal 8)
        (loop (fft signal) (add1 count)))))

(define (fft-half ns)
  (second
   (foldr (λ (v acc)
            (match-let ([(list sum lst) acc])
              (let ([sum (% 10 (+ v sum))])
                (list sum (cons (abs sum) lst)))))
          (list 0 '(0)) ns)))

(define (part2)
  (let* ([offset (digits->number (take input 7))]
         [input-long (repeat 10000 input)]
         [half-length (/ (* 10000 (length input)) 2)]
         [input-half (drop input-long half-length)])
    (let loop ([count 0]
               [signal input-half])
      (if (= count 100)
          (take (drop signal (- offset half-length)) 8)
          (loop (add1 count) (fft-half signal))))))