#lang racket

(require "../lib.rkt"
         graph)

;; Given a list of strings of the form "XXX(YYY",
;; convert it into a list of '(1 XXX YYY),
;; representing a weighted edge graph from XXX to YYY of weight 1
;; so the distance between two vertices is the number of orbit jumps
(define input
  (map (λ (str)
         (list 1
               (string->symbol (substring str 0 3))
               (string->symbol (substring str 4 7))))
       (problem-input 6)))

(define input-dag
  (weighted-graph/directed input))

(define input-udag
  (weighted-graph/undirected input))

;; The number of orbits (direct and indirect) of a satellite is
;; its distance from 'COM, so the total number of orbits is
;; the sum of the distances to all satellites
(define part1
  (let-values ([(dist-hash _)
                (dag-shortest-paths input-dag 'COM)])
    (sum (hash-values dist-hash))))

;; The call to fewest-vertices-path will give the number of vertices
;; between 'YOU and 'SAN inclusive, so subtract 1 to get the distance
;; But we want the number of orbital transfers, i.e. the distance from
;; the object 'YOU orbits to the object 'SAN orbits, so subtract 2
(define part2
  (- (length (fewest-vertices-path input-udag 'YOU 'SAN)) 3))

(show-solution part1 part2)