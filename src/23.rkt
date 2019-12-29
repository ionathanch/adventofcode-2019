#lang plai

(require data/queue
         racket/set
         "../lib.rkt"
         "IntCode.rkt")

(define input
  (string->program (car (problem-input 23))))

(define network
  (build-vector 50 (λ (n) (resume-with-input (resume-with-input (exec input) n) -1))))

(define packets
  (build-vector 50 (λ (_) (make-queue))))

(define (waiting? st)
  (type-case state st
    [in (_) #t]
    [else #f]))

(define part2
  (let/cc k
    (let ([prev-NATx 0]
          [prev-NATy 0]
          [NATx -1]
          [NATy -1])
      (let loop ()
        (for ([i (range 0 50)])
          (let ([st (vector-ref network i)]
                [input (vector-ref packets i)])
            (type-case state st
              [in (resume)
                  (if (queue-empty? input)
                      (void)
                      (let* ([x (dequeue! input)]
                             [y (dequeue! input)]
                             [st (resume-with-input (resume x) y)])
                        (vector-set! network i st)))]
              [out (j resume)
                   (let*-values ([(x st) (resume-with-output (resume))]
                                 [(y st) (resume-with-output st)])
                     (if (= j 255)
                         (begin
                           (when (= NATy -1)
                             (printf "Part 1: ~a\n" y))
                           (set! NATx x)
                           (set! NATy y))
                         (begin
                           (enqueue! (vector-ref packets j) x)
                           (enqueue! (vector-ref packets j) y)))
                     (vector-set! network i st))]
              [halt (_) (error "Unexpected program state.")])))
        (when (and (andmap (∘ waiting? (∂ vector-ref network)) (range 0 50))
                   (andmap queue-empty? (vector->list packets)))
          (when (= NATy prev-NATy) (k NATy))
          (let ([addr0 (vector-ref packets 0)])
            (set! prev-NATx NATx)
            (set! prev-NATy NATy)
            (enqueue! addr0 NATx)
            (enqueue! addr0 NATy)))
        (loop)))))

(printf "Part 2: ~a\n" part2)