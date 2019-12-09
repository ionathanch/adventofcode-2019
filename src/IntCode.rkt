#lang plai

(require racket/vector
         "../lib.rkt")

(define (vector-ref** vec pos)
  (vector-ref* vec pos 0))

;; string->program : string -> (listof number)
;; A program is a list of numbers,
;; which are sequences of instructions and parameters.
(define (string->program str)
  (list->vector (map string->number (string-split str ","))))

(define (program? p)
  (vectorof number?))

;; state =
;;       | out number (() -> state)
;;       | in (number -> state)
;;       | halt program
(define-type state
  (out [value number?] [resume procedure?])
  (in  [resume procedure?])
  (halt [program program?]))

;; resume-with-output : state -> (values number state)
;; Extract the output, then resume the program.
(define (resume-with-output st)
  (type-case state st
    [out (value resume) (values value (resume))]
    [else (error "resume-with-output: Unexpected program state.")]))

;; resume-with-input : state -> number -> state
;; Resume the program with given input.
(define (resume-with-input st input)
  (type-case state st
    [in (resume) (resume input)]
    [else (error "resume-with-input: Unexpected program state.")]))

;; resume-with-io : state -> (listof number) -> (listof number)
;; Run the program, providing input as needed, and collecting output.
(define (resume-with-io st inputs)
  (type-case state st
    [in (resume)
        (resume-with-io (resume (car inputs)) (cdr inputs))]
    [out (value resume)
         (cons value (resume-with-io (resume) inputs))]
    [halt (program) '()]))

;; halt-with-program : state -> program
;; Return program state of halted execution.
(define (halt-with-program st)
  (type-case state st
    [halt (program) program]
    [else (error "halt-with-program: Unexpected program state.")]))

;; exec* : program -> number -> number -> state
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
(define (exec* program #:ptr [pointer 0] #:base [base 0])
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
         (exec* program #:ptr next-pointer #:base base))]
      [3
       (let* ([resume
               (λ (input)
                 (vector-set!* program (l1) input)
                 (exec* program #:ptr next-pointer #:base base))])
         (in resume))]
      [4
       (let* ([output (v1)]
              [resume
               (λ () (exec* program #:ptr next-pointer #:base base))])
         (out output resume))]
      [(or 5 6)
       (let* ([jump-if (match opcode [5 nzero?] [6 zero?])]
              [next-pointer (if (jump-if (v1)) (v2) next-pointer)])
         (exec* program #:ptr next-pointer #:base base))]
      [(or 7 8)
       (let* ([lt-eq (match opcode [7 <] [8 =])]
              [value (if (lt-eq (v1) (v2)) 1 0)]
              [program (vector-set!* program (l3) value)])
         (exec* program #:ptr next-pointer #:base base))]
      [9
       (let ([base (+ base (v1))])
         (exec* program #:ptr next-pointer #:base base))]
      [99
       (halt program)])))

;; Just so we always run the program on a fresh copy
(define (exec program #:ptr [pointer 0] #:base [base 0])
  (exec* (vector-copy program) #:ptr pointer #:base base))