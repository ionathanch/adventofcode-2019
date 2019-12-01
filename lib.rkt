#lang racket

(require racket/format
         2htdp/batch-io)

(provide problem-input
         show-solution
         sum)

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