#lang plai

(require data/queue
         "../lib.rkt"
         "IntCode.rkt")

(define input
  (string->program (car (problem-input 7))))

(define (amplify phase)
  (let* ([outA (resume-with-io (exec input) (list (first  phase) 0))]
         [outB (resume-with-io (exec input) (list (second phase) (car outA)))]
         [outC (resume-with-io (exec input) (list (third  phase) (car outB)))]
         [outD (resume-with-io (exec input) (list (fourth phase) (car outC)))]
         [outE (resume-with-io (exec input) (list (fifth  phase) (car outD)))])
    (car outE)))

(define part1
  (let ([phases (permutations '(0 1 2 3 4))])
    (apply max (append (map amplify phases)))))

(define (amplify-loop phase)
  (let* ([amps (map (curry resume-with-input (exec input)) phase)]
         [Q (list->queue amps)])
    (let loop ([signal 0])
      (define amp (dequeue! Q))
      (type-case state amp
        [halt (_) signal]
        [in (resume)
            (define-values (signal st)
              (resume-with-output (resume signal)))
            (enqueue! Q st)
            (loop signal)]
        [else (error "amplify-loop: Unexpected program state.")]))))

(define part2
  (let ([phases (permutations '(5 6 7 8 9))])
    (apply max (map amplify-loop phases))))

(show-solution part1 part2)