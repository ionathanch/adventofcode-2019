#lang racket

(require data/queue
         "../lib.rkt"
         "IntCode.rkt")

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

(struct state (program pointer input) #:transparent)

(define (exec-state program #:ptr [pointer 0] #:in [input '()])
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
         (exec-state program #:ptr next-pointer #:in input))]
      [3
       (let* ([value (car input)]
              [input (cdr input)]
              [program (list-set program (l1) value)])
         (exec-state program #:ptr next-pointer #:in input))]
      [4
       (values 'yield (v1)
               (state program next-pointer input))]
      [(or 5 6)
       (let* ([jump-if (match opcode [5 nzero?] [6 zero?])]
              [next-pointer (if (jump-if (v1)) (v2) next-pointer)])
         (exec-state program #:ptr next-pointer #:in input))]
      [(or 7 8)
       (let* ([lt-eq (match opcode [7 <] [8 =])]
              [value (if (lt-eq (v1) (v2)) 1 0)]
              [program (list-set program (l3) value)])
         (exec-state program #:ptr next-pointer #:in input))]
      [99
       (values 'halt (car input)
               (state program next-pointer (cdr input)))])))

(define (amplify-loop phase)
  (let* ([input (vector->list input)]
         [amps (map (compose (curry state input 0) list) phase)]
         [Q (make-queue)])
    (map (curry enqueue! Q) amps)
    (let loop ([signal 0])
      (let* ([amp (dequeue! Q)])
        (let-values ([(code signal amp)
                      (exec-state (state-program amp)
                            #:ptr (state-pointer amp)
                            #:in (rac (state-input amp) signal))])
          (match code
            ['yield (enqueue! Q amp) (loop signal)]
            ['halt signal]))))))

(define part2
  (let ([phases (permutations '(5 6 7 8 9))])
    (apply max (map amplify-loop phases))))

(show-solution part1 part2)