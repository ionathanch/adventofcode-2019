#lang racket

(require "../lib.rkt")

;; string->program : string -> (listof number)
;; A program is a list of numbers,
;; which are sequences of instructions and parameters.
(define (string->program str)
  (map string->number (string-split str ",")))

(define input
  (string->program (car (problem-input 5))))

(struct instruction
  (opcode mode1 mode2 mode3)
  #:transparent)

;; fetch : number -> number -> program -> program
;; If the mode is 0, the value at pointer is an address.
;; If the mode is 1, the value at pointer is immediate.
;; Return the value at that address or the immediate.
(define (fetch mode pointer program)
  (if (zero? mode)
      (list-ref program (list-ref program pointer))
      (list-ref program pointer)))

;; write : number -> number -> program -> program
;; Write the value into the location at pointer.
(define (write value pointer program)
  (list-set program (list-ref program pointer) value))

;; decode-instr : number -> instruction
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
;; Note that leading zeroes in the encoded instruction are omitted.
(define (decode-instr n)
  (let* ([cs (number->digits-reverse n)]
         [ops '(1 2 3 4 5 6 7 8 99)]
         [op? (λ (v) (member v ops))])
    (match cs
      [(list-rest (? op? op) rest)
       (instruction op
                    (list-ref* rest 1 0)
                    (list-ref* rest 2 0)
                    (list-ref* rest 3 0))]
      [(list-rest 9 9 _)
       (instruction 99 0 0 0)])))

;; exec : number -> program -> program
(define (exec pointer program)
  (let* ([instr (decode-instr (list-ref program pointer))]
         [v1 (λ () (fetch (instruction-mode1 instr) (+ pointer 1) program))]
         [v2 (λ () (fetch (instruction-mode2 instr) (+ pointer 2) program))]
         [next-pointer
          (λ (op) (match op
                    [(or 1 2 7 8) (+ pointer 4)]
                    [(or 3 4) (+ pointer 2)]
                    [(or 5 6) (+ pointer 3)]))]
         [arith
          (λ (op) (match op [1 +] [2 *]))]
         [jump-if
          (λ (op) (match op [5 nzero?] [6 zero?]))]
         [lt-eq
          (λ (op) (match op [7 <] [8 =]))])
    (match (instruction-opcode instr)
      [(and op (or 1 2))
       (let* ([program (write ((arith op) (v1) (v2)) (+ pointer 3) program)])
         (exec (next-pointer op) program))]
      [3
       (let* ([input (read)]
              [program (write input (+ pointer 1) program)])
         (exec (next-pointer 3) program))]
      [4
       (displayln (v1))
       (exec (next-pointer 4) program)]
      [(and op (or 5 6))
       (let* ([pointer (if ((jump-if op) (v1)) (v2)
                           (next-pointer op))])
         (exec pointer program))]
      [(and op (or 7 8))
       (let* ([v3 (if ((lt-eq op) (v1) (v2)) 1 0)]
              [program (write v3 (+ pointer 3) program)])
         (exec (next-pointer op) program))]
      [99 program])))

(define (execute)
  (exec 0 input)
  (void))