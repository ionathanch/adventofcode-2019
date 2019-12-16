#lang plai

(require graph
         data/queue
         (except-in "../lib.rkt" transpose)
         "IntCode.rkt")

(define input
  (string->program (car (problem-input 15))))

;; grid : (hashof (coord => status))
;; We will use a hashtable from coordinates to status codes:
;; 0: wall
;; 1: floor
;; 2: oxygen system
;; 3: unexplored
;; 4: droid
;; Use (show-hash-grid status-hash grid 3) to print out the grid as a map
(define status-hash (make-hash '((0 . #\#) (1 . #\.) (2 . #\O) (3 . #\ ) (4 . #\D))))

;; coord : (number . number)
;; Coordinates increase in the southern and eastern directions

;; next-doors : dir -> coord -> (listof coord)
;; Give a list of the next coordinates we could visit
(define (next-coords coord)
  (let* ([x (car coord)]
         [y (cdr coord)])
    (list (cons x (sub1 y))
          (cons x (add1 y))
          (cons (sub1 x) y)
          (cons (add1 x) y))))

;; next-dir : coord -> coord -> dir
;; Given our old coordinate and the new coordinate,
;; return the direction we have gone in
(define (next-dir old-coord new-coord)
  (let ([old-x (car old-coord)]
        [old-y (cdr old-coord)]
        [new-x (car new-coord)]
        [new-y (cdr new-coord)])
    (cond
      [(< new-y old-y) 1]    ; north
      [(> new-y old-y) 2]    ; south
      [(< new-x old-x) 3]    ; west
      [(> new-x old-x) 4]))) ; east

;; move-list : coord -> state -> grid -> (list grid (listof procedure))
;; For each possible direction we can move in, return an update grid
;; and a list of functions, each of which are advancing the state
;; from the next possible coordinate when given a grid
;; I'm not sure how to assign a type to this...
(define (move-list coord st grid)
  (let ([unvisited (filter-not (∂ hash-has-key? grid) (next-coords coord))])
    (foldl
     (λ (next grid-lst)
       (match-let ([(list grid lst) grid-lst])
         (type-case state (resume-with-input st (next-dir coord next))
           [out (value resume)
                (list (hash-set grid next value)
                      (if (= 0 value) lst
                          (cons (∂ move-list next (resume)) lst)))]
           [else (error "Unexpected program state")])))
     (list grid '()) unvisited)))

;; explore-map : grid -> grid
;; Breadth-first search of all accessible locations
(define (explore-map grid)
  (let ([Q (make-queue)])
    (enqueue! Q (∂ move-list '(0 . 0) (exec input)))
    (let loop ([grid grid])
      (if (queue-empty? Q) grid
          (match-let ([(list grid nexts)
                       ((dequeue! Q) grid)])
            (for-each (∂ enqueue! Q) nexts)
            (loop grid))))))

;; grid->graph : grid -> unweighted, undirected graph between coordinates
;; There is an edge between coordinates if we can travel from one to the other
(define (grid->graph grid)
  (define (filter-non-wall coords)
    (filter (λ (c) (!= 0 (hash-ref grid c 0))) coords))
  (unweighted-graph/undirected
   (apply append
          (map (λ (coord)
                 (map (∂ list coord)
                      (filter-non-wall (next-coords coord))))
               (filter-non-wall (hash-keys grid))))))

(define-values (part1 part2)
  (let* ([grid (make-immutable-hash '(((0 . 0) . 4)))]
         [grid (explore-map grid)]
         [oxygen (car (findf (∘ (∂ = 2) cdr) (hash->list grid)))]
         [graph (grid->graph grid)])
    (show-hash-grid status-hash grid 3)
    (let-values ([(dist-hash _) (dijkstra graph oxygen)])
      (values (hash-ref dist-hash '(0 . 0))
              (apply max (hash-values dist-hash))))))

(show-solution part1 part2)