#lang racket

(require "../lib.rkt"
         "05.rkt")

(define input
  (string->program (car (problem-input 7))))

(define (amplify phase)
  (let*-values ([(_ outA) (exec input #:in (list (first  phase) 0))]
                [(_ outB) (exec input #:in (list (second phase) (car outA)))]
                [(_ outC) (exec input #:in (list (third  phase) (car outB)))]
                [(_ outD) (exec input #:in (list (fourth phase) (car outC)))]
                [(_ outE) (exec input #:in (list (fifth  phase) (car outD)))])
    (car outE)))

(define part1
  (let ([phases (permutations '(0 1 2 3 4))])
    (apply max (append (map amplify phases)))))

;; The IntCode interpreter from Day 5, except it uses threads
(define (exec-pipe program #:ptr [pointer 0] #:thr thread)
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
         (exec-pipe program #:ptr next-pointer #:thr thread))]
      [3
       (let* ([value (thread-receive)]
              [program (list-set program (l1) value)])
         (exec-pipe program #:ptr next-pointer #:thr thread))]
      [4
       (thread-send thread (v1))
       (exec-pipe program #:ptr next-pointer #:thr thread)]
      [(or 5 6)
       (let* ([jump-if (match opcode [5 nzero?] [6 zero?])]
              [next-pointer (if (jump-if (v1)) (v2) next-pointer)])
         (exec-pipe program #:ptr next-pointer #:thr thread))]
      [(or 7 8)
       (let* ([lt-eq (match opcode [7 <] [8 =])]
              [value (if (lt-eq (v1) (v2)) 1 0)]
              [program (list-set program (l3) value)])
         (exec-pipe program #:ptr next-pointer #:thr thread))]
      [99
       (thread-send thread (values program (thread-receive)))])))

(define (amplify-loop phase)
  (let* ([threadE (thread (exec-pipe input #:thr (current-thread)))]
         [threadD (thread (exec-pipe input #:thr threadE))]
         [threadC (thread (exec-pipe input #:thr threadD))]
         [threadB (thread (exec-pipe input #:thr threadC))]
         [threadA (thread (exec-pipe input #:thr threadB))])
    (thread-send threadE (fifth  phase))
    (thread-send threadD (fourth phase))
    (thread-send threadC (third  phase))
    (thread-send threadB (second phase))
    (thread-send threadA (first phase))
    (thread-send threadA 0)
    (let loop ()
      (let ([msg (thread-receive)])
        (if (number? msg)
            (begin
              (thread-send threadA msg)
              (loop))
            (let-values ([(_ output) msg])
              output))))))

(show-solution part1 #f)