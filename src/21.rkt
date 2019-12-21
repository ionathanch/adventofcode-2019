#lang racket

(require "../lib.rkt"
         "IntCode.rkt")

(define input
  (string->program (car (problem-input 21))))

(define (asciis->integers asciis)
  (map char->integer
       (string->list
        (string-append
         (string-join asciis "\n") "\n"))))

(define (integers->asciis ints)
  (list->string (map integer->char ints)))

;; !(A ∧ B ∧ C) ∧ D
;; Jump if any of the next three tiles are holes
;; and the fourth tile is ground.
(define part1
  (let ([springscript '("OR A J"
                        "AND B J"
                        "AND C J"
                        "NOT J J"
                        "AND D J"
                        "WALK")])
    (last (resume-with-io (exec input) (asciis->integers springscript)))))

;; !(A ∧ B ∧ C) ∧ D ∧ (E ∨ H)
;; Jump if any of the next three tiles are holes (A, B, C),
;; but only if the fourth tile is ground (D),
;; and there is another tile to reach after that,
;; either by running (E) or by jumping (H)
(define part2
  (let ([springscript '("OR A J"
                        "AND B J"
                        "AND C J"
                        "NOT J J"
                        "AND D J"
                        "OR E T"
                        "OR H T"
                        "AND T J"
                        "RUN")])
    (last (resume-with-io (exec input) (asciis->integers springscript)))))

(show-solution part1 part2)