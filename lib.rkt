#lang racket

(require racket/format
         2htdp/batch-io)

(provide problem-input
         show-solution
         sum
         neq?
         nzero?
         list-ref*
         number->digits-reverse
         number->digits)

;; IO helpers

;; problem-input : number? -> (listof string?)
;; Return contents of input file input/xx.txt as lines of strings.
(define (problem-input n)
  (let* ([filename (~a n #:min-width 2 #:align 'right #:left-pad-string "0")]
         [path     (string-append "../input/" filename ".txt")])
    (read-lines path)))

;; show-solution : any/c -> any/c -> void
;; Print part1 and part2 on separate lines.
(define (show-solution part1 part2)
  (printf "Part 1: ~a\nPart 2: ~a\n" part1 part2))


;; Common helpers

;; sum : (listof number) -> number
(define (sum ns) (apply + ns))

;; neq : any -> any -> boolean
(define (neq? v1 v2)
  (not (eq? v1 v2)))

;; nzero? : number -> boolean
(define (nzero? n)
  (not (zero? n)))

;; list-ref* : (listof any) -> number -> any -> any
;; Same as list-ref, except a default value is provided
;; if the index is beyond the length of the list.
(define (list-ref* lst pos failure-result)
  (if (>= pos (length lst))
      failure-result
      (list-ref lst pos)))

;; number->digits-reverse : number -> (listof number)
;; Return the digits of the given number in reverse order (i.e. RTL)
(define (number->digits-reverse n)
  (if (< n 10)
      (list n)
      (cons (remainder n 10)
            (number->digits-reverse (quotient n 10)))))

;; number->digits : number -> (listof number)
;; Return the digits of the given number (LTR)
(define (number->digits n)
  (reverse (number->digits-reverse n)))