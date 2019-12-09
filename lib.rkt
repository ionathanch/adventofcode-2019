#lang racket

(require racket/format
         2htdp/batch-io)

(provide problem-input
         show-solution
         sum
         neq?
         nzero?
         number->digits-reverse
         number->digits
         rac
         list-ref*
         chunks-of
         transpose
         vector-ref*
         vector-set!*)

;; IO helpers ;;

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


;; Number helpers ;;

;; sum : (listof number) -> number
(define (sum ns) (apply + ns))

;; neq : any -> any -> boolean
(define (neq? v1 v2)
  (not (eq? v1 v2)))

;; nzero? : number -> boolean
(define (nzero? n)
  (not (zero? n)))

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


;; List helpers ;;

;; rac : (listof any) -> any -> (listof any)
;; Append element to the back of the list.
(define (rac lst v)
  (append lst (list v)))

;; list-ref* : (listof any) -> number -> any -> any
;; Same as list-ref, except a default value is provided
;; if the index is beyond the length of the list.
(define (list-ref* lst pos failure-result)
  (if (>= pos (length lst))
      failure-result
      (list-ref lst pos)))

;; chunks-of : (listof any) -> nonzero? -> (listof (listof any))
;; Partitions a list into lists of the given size in order,
;; with the final list possibly being smaller
;; e.g. '(1 2 3 4 5) 2 => '((1 2) (3 4) (5))
(define (chunks-of lst size)
  (if (< (length lst) size) lst
      (cons (take lst size)
            (chunks-of (drop lst size) size))))

;; transpose : (listof (listof any)) -> (listof (listof any))
;; Turns a list of lists into a list of lists of
;; the first elements of the lists, ..., the nth elements
;; where n is the length of the shortest list.
;; In short, it transposes a list of rows into a list of columns.
;; e.g. '((1 2 3 4)          '((1 5 8)
;;        (5 6 7)         =>   (2 6 9)
;;        (8 9 10 11 12))      (3 7 10))
(define (transpose layers)
  (if (ormap empty? layers) '()
      (let ([pixels (map car layers)]
            [layers (map cdr layers)])
        (cons pixels (transpose layers)))))


;; Vector helpers ;;

;; vector-ref* : (vectorof any) -> number -> any -> any
;; Same as list-ref, except a default value is provided
;; if the index is beyond the length of the list.
(define (vector-ref* vec pos failure-result)
  (if (>= pos (vector-length vec))
      failure-result
      (vector-ref vec pos)))

;; vector-set!* : (vectorof any) -> number -> any -> (vectorof any)
;; Set the value at given index in the vector, then return the vector
;; If the index is beyond the length of the vector,
;; a vector that can accomodate that index is returned,
;; with all the original elements and the element at the index set
(define (vector-set!* vec pos v)
  (if (< pos (vector-length vec))
      (begin (vector-set! vec pos v) vec)
      (let ([new-vec (make-vector (add1 pos))])
        (vector-copy! new-vec 0 vec)
        (vector-set! new-vec pos v)
        new-vec)))