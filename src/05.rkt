#lang racket

(require "../lib.rkt")

;; string->program : string -> (listof number)
;; A program is a list of numbers,
;; which are sequences of instructions and parameters.
(define (string->program str)
  (map string->number (string-split str ",")))

(define input
  (string->program (car (problem-input 5))))

;; exec : number -> program -> program
;; An encoded instruction is anywhere from 1 to 4 digits long.
;; The last one or two digits represent the opcode, which can be:
;;   - 1/2: add/multiply parameters 1 and 2 and store in parameter 3
;;   - 3:   take an input and store in parameter 1
;;   - 4:   output parameter 1
;;   - 5/6: if parameter 1 is non-zero/zero, jump to parameter 2
;;   - 7/8: if parameter 1 is less-than/equal-to parameter 2,
;;          store 1 else store 0 in parameter 3
;;   - 99:  halt
;; The next few digits to the left of the opcode (if any) represent
;; the mode of each parameter, with that of parameter i in the digit
;; i digits to the left of the opcode.
;; If the mode is 0, the value at pointer is an address.
;; If the mode is 1, the value at pointer is immediate.
;; Note that leading zeroes in the encoded instruction are omitted.
(define (exec pointer program)
  (let* ([instruction (list-ref program pointer)]
         [opcode (remainder instruction 100)]
         [mode1 (remainder (quotient instruction 100) 10)]
         [mode2 (remainder (quotient instruction 1000) 10)]
         [mode3 (remainder (quotient instruction 10000) 10)]
         ;; l* : call to get write location from program
         [l1 (λ () (if (zero? mode1) (list-ref program (+ pointer 1)) (+ pointer 1)))]
         [l2 (λ () (if (zero? mode2) (list-ref program (+ pointer 2)) (+ pointer 2)))]
         [l3 (λ () (if (zero? mode3) (list-ref program (+ pointer 3)) (+ pointer 3)))]
         ;; v* : call to read values from program
         [v1 (λ () (list-ref program (l1)))]
         [v2 (λ () (list-ref program (l2)))]
         [v3 (λ () (list-ref program (l3)))]
         [next-pointer
          (match opcode
            [(or 1 2 7 8) (+ pointer 4)]
            [(or 3 4) (+ pointer 2)]
            [(or 5 6) (+ pointer 3)]
            [99 (+ pointer 1)])])
    (match opcode
      [(or 1 2)
       (let* ([arith (match opcode [1 +] [2 *])]
              [value (arith (v1) (v2))]
              [program (list-set program (l3) value)])
         (exec next-pointer program))]
      [3
       (let* ([program (list-set program (l1) (read))])
         (exec next-pointer program))]
      [4
       (displayln (v1))
       (exec next-pointer program)]
      [(or 5 6)
       (let* ([jump-if (match opcode [5 nzero?] [6 zero?])]
              [next-pointer (if (jump-if (v1)) (v2) next-pointer)])
         (exec next-pointer program))]
      [(or 7 8)
       (let* ([lt-eq (match opcode [7 <] [8 =])]
              [value (if (lt-eq (v1) (v2)) 1 0)]
              [program (list-set program (l3) value)])
         (exec next-pointer program))]
      [99 program])))

(define (execute)
  (exec 0 input)
  (void))