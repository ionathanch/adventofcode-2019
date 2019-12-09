#lang racket

(require racket/vector
         "../lib.rkt")

(provide string->program
         exec)

(define (vector-ref** vec pos)
  (vector-ref* vec pos 0))

;; string->program : string -> (listof number)
;; A program is a list of numbers,
;; which are sequences of instructions and parameters.
(define (string->program str)
  (list->vector (map string->number (string-split str ","))))

;; exec* : program -> number -> number -> (listof number) -> (listof number) -> program
;; An encoded instruction is anywhere from 1 to 4 digits long.
;; The last one or two digits represent the opcode, which can be:
;;   - 1/2: add/multiply parameters 1 and 2 and store in parameter 3
;;   - 3:   take an input and store in parameter 1
;;   - 4:   output parameter 1
;;   - 5/6: if parameter 1 is non-zero/zero, jump to parameter 2
;;   - 7/8: if parameter 1 is less-than/equal-to parameter 2,
;;          store 1 else store 0 in parameter 3
;;   - 9:   add parameter 1 to relative base
;;   - 99:  halt
;; The next few digits to the left of the opcode (if any) represent
;; the mode of each parameter, with that of parameter i in the digit
;; i digits to the left of the opcode.
;; If the mode is 0, the value at pointer is an address.
;; If the mode is 1, the value at pointer is immediate.
;; If the mode is 2, the value at pointer is an address to be offset by base.
;; Note that leading zeroes in the encoded instruction are omitted.
(define (exec* program #:ptr [pointer 0] #:base [base 0] #:in [input '()] #:out [output '()])
  (define instruction (vector-ref** program pointer))
  (define opcode (remainder instruction 100))
  (define next-pointer
    (match opcode
      [(or 1 2 7 8) (+ pointer 4)]
      [(or 3 4 9) (+ pointer 2)]
      [(or 5 6) (+ pointer 3)]
      [99 (+ pointer 1)]))
  (define (get-location index mode)
    (match mode
      [0 (vector-ref** program (+ pointer index))]
      [1 (+ pointer index)]
      [2 (+ (vector-ref** program (+ pointer index)) base)]))
  (let* ([mode1 (remainder (quotient instruction 100) 10)]
         [mode2 (remainder (quotient instruction 1000) 10)]
         [mode3 (remainder (quotient instruction 10000) 10)]
         ;; l* : call to get write location from program
         [l1 (λ () (get-location 1 mode1))]
         [l2 (λ () (get-location 2 mode2))]
         [l3 (λ () (get-location 3 mode3))]
         ;; v* : call to read values from program
         [v1 (λ () (vector-ref** program (l1)))]
         [v2 (λ () (vector-ref** program (l2)))]
         [v3 (λ () (vector-ref** program (l3)))])
    (match opcode
      [(or 1 2)
       (let* ([arith (match opcode [1 +] [2 *])]
              [value (arith (v1) (v2))]
              [program (vector-set!* program (l3) value)])
         (exec* program #:ptr next-pointer #:base base #:in input #:out output))]
      [3
       (let* ([value (car input)]
              [input (cdr input)]
              [program (vector-set!* program (l1) value)])
         (exec* program #:ptr next-pointer #:base base #:in input #:out output))]
      [4
       (let* ([output (append output `(,(v1)))])
         (exec* program #:ptr next-pointer #:base base #:in input #:out output))]
      [(or 5 6)
       (let* ([jump-if (match opcode [5 nzero?] [6 zero?])]
              [next-pointer (if (jump-if (v1)) (v2) next-pointer)])
         (exec* program #:ptr next-pointer #:base base #:in input #:out output))]
      [(or 7 8)
       (let* ([lt-eq (match opcode [7 <] [8 =])]
              [value (if (lt-eq (v1) (v2)) 1 0)]
              [program (vector-set!* program (l3) value)])
         (exec* program #:ptr next-pointer #:base base #:in input #:out output))]
      [9
       (let ([base (+ base (v1))])
         (exec* program #:ptr next-pointer #:base base #:in input #:out output))]
      [99 (values program output)])))

;; Just so we always run the program on a fresh copy
(define (exec program #:ptr [pointer 0] #:base [base 0] #:in [input '()] #:out [output '()])
  (exec* (vector-copy program) #:ptr pointer #:base base #:in input #:out output))