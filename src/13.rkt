#lang plai

(require "../lib.rkt"
         "IntCode.rkt")

(define input
  (string->program (car (problem-input 13))))

(define (take-every n lst)
  (if (< (length lst) n) '()
      (let ([dropped (drop lst (sub1 n))])
        (cons (car dropped)
              (take-every n (cdr dropped))))))

(define-values (output part1)
  (let ([output (resume-with-io (exec input) '())])
    (values output
            (count (∂ = 2) (take-every 3 output)))))

(define width
  (apply max (take-every 1 output)))

(define height
  (apply max (take-every 2 output)))

(define grid
  (build-vector
   (add1 height)
   (λ (_) (make-vector (add1 width)))))

(define score 0)
(define paddle-x 0)
(define ball-x 0)

(define tile-hash
  (make-hash
   '((0 . #\ )
     (1 . #\█)
     (2 . #\▒)
     (3 . #\‗)
     (4 . #\●))))

(define (draw-grid)
  (printf "Score: ~a\n" score)
  (show-msg tile-hash (take (map vector->list (vector->list grid)) 26)))

(define (update-grid! x y t)
  (cond
    [(= t 3) (set! paddle-x x)]
    [(= t 4) (set! ball-x   x)])
  (if (and (= x -1) (= y 0))
      (set! score t)
      (vector-set! (vector-ref grid y) x t)))

(define updator
  (∂ (λ (x y t) (update-grid! x y t) updator)))

(define (play)
  (vector-set! input 0 2)
  (let loop ([update! updator]
             [st (exec input)])
    (type-case state st
      [halt (_) (printf "Game over!\n")]
      [out (tile resume)
           (loop (update! tile) (resume))]
      [in (resume)
          (let ([move (cond
                        [(< paddle-x ball-x) 1]
                        [(= paddle-x ball-x) 0]
                        [(> paddle-x ball-x) -1])])
            (loop update! (resume move)))])))

;; Interactive version of play uses the below in type-case `in`
#;(begin
    (draw-grid)
    (loop output (resume (read))))

(define part2
  (begin (play) score))

(show-solution part1 part2)