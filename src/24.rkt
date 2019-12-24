#lang racket

(require racket/set
         "../lib.rkt")

(define input
  (problem-input 24))

;; coord : (list number number)
;; grid : (vectorof (vectorof char))
;; side : ('U | 'D | 'L | 'R)

(define width
  (string-length (car input)))

(define height
  (length input))

(define mid-x
  (round (/ width 2)))

(define mid-y
  (round (/ height 2)))

(define vector-grid
  (list->vector (map (∘ list->vector string->list) input)))

;; make-empty-grid : -> grid
;; Create a (height × width) vector grid of #\.
(define (make-empty-grid)
  (build-vector height (λ (_) (make-vector width #\.))))

;; outer-edge? : coord -> boolean
;; Return whether the given coordinate is on the edge of the grid
(define (outer-edge? coord)
  (match-let ([(list x y) coord])
    (or (= x 0) (= y 0)
        (= x (sub1 width)) (= y (sub1 height)))))

;; inner-edge? : coord -> boolean
;; Return whether the given coordinate is adjacent to the centre tile
(define (inner-edge? coord)
  (match-let ([(list x y) coord])
    (or (and (= x mid-x) (<= (sub1 mid-y) y (add1 mid-y)))
        (and (= y mid-y) (<= (sub1 mid-x) x (add1 mid-x))))))

;; edge-bugs : grid -> side -> number
;; Return the number of bugs on the top/bottom/left/right edge
(define (edge-bugs grid side)
  (match side
    ['U (vector-count (∂ char=? #\#) (vector-ref grid 0))]
    ['D (vector-count (∂ char=? #\#) (vector-ref grid (sub1 height)))]
    ['L (vector-count (∂ char=? #\#) (vector-map (∂ vector-first) grid))]
    ['R (vector-count (∂ char=? #\#) (vector-map (∂ vector-last)  grid))]))

;; edges-bugs? : grid -> boolean
;; Return whether any of the edges have bugs
(define (edges-bugs? grid)
  (!= 0 (+ (edge-bugs grid 'U)
           (edge-bugs grid 'D)
           (edge-bugs grid 'L)
           (edge-bugs grid 'R))))

;; middle-bug : grid -> side -> number
;; Return the number of bugs on the tile above/below/leftof/rightof the centre tile
(define (middle-bug grid side)
  (match side
    ['U (if (char=? #\# (vector-grid-ref* grid (list mid-x (sub1 mid-y)) #\.)) 1 0)]
    ['D (if (char=? #\# (vector-grid-ref* grid (list mid-x (add1 mid-y)) #\.)) 1 0)]
    ['L (if (char=? #\# (vector-grid-ref* grid (list (sub1 mid-x) mid-y) #\.)) 1 0)]
    ['R (if (char=? #\# (vector-grid-ref* grid (list (add1 mid-x) mid-y) #\.)) 1 0)]))

;; middle-bugs? : grid -> boolean
;; Return whether any of the tiles adjacent to the centre tile has bugs
(define (middle-bugs? grid)
  (!= 0 (+ (middle-bug grid 'U)
           (middle-bug grid 'D)
           (middle-bug grid 'L)
           (middle-bug grid 'R))))

;; adjacent-bugs : grid -> coord -> number
;; Count the number of bugs adjacent to the given tile
(define (adjacent-bugs grid coord)
  (match-let* ([(list x y) coord]
               [U (vector-grid-ref* grid (list x (sub1 y)) #\.)]
               [D (vector-grid-ref* grid (list x (add1 y)) #\.)]
               [L (vector-grid-ref* grid (list (sub1 x) y) #\.)]
               [R (vector-grid-ref* grid (list (add1 x) y) #\.)])
    (count (∂ char=? #\#) (list U D L R))))

;; adjacent-bugs* : level -> grid -> coord
;; Count the number of bugs adjacent to the given tile
;; on the grid on the given level
;; However! If the coordinate is on the edge, instead of treating the other side
;; like empty space, we look at the corresponding tile adjacent to the centre tile
;; on the previous level's grid, as if the current grid were embedded
;; Furthermore! If the coordinate is adjacent to the centre tile, instead of looking
;; at the centre tile, we look at the corresponding edge of the next level's grid,
;; as if that grid were embedded in the current one.
;; By "corresponding", it means, for instance, if the current coordinate were on
;; the top edge, we need to look at the tile above the centre tile in the previous grid;
;; if the current coordinate were above the centre tile, we need to look at the top edge
;; of the next grid.
(define (adjacent-bugs* level grids coord)
  (match-let* ([(list x y) coord]
               [grid (list-ref grids level)]
               [level-bugs (adjacent-bugs grid coord)])
    (cond
      [(and (outer-edge? coord) (> level 0))
       (let* ([outer-grid (list-ref grids (sub1 level))]
              [U (middle-bug outer-grid 'U)]
              [D (middle-bug outer-grid 'D)]
              [L (middle-bug outer-grid 'L)]
              [R (middle-bug outer-grid 'R)])
         (cond
           [(and (= x 0) (= y 0))
            (+ level-bugs L U)]
           [(and (= x 0) (= y (sub1 height)))
            (+ level-bugs L D)]
           [(and (= x (sub1 width)) (= y 0))
            (+ level-bugs R U)]
           [(and (= x (sub1 width)) (= y (sub1 height)))
            (+ level-bugs R D)]
           [(= x 0)
            (+ level-bugs (middle-bug outer-grid 'L))]
           [(= y 0)
            (+ level-bugs (middle-bug outer-grid 'U))]
           [(= x (sub1 width))
            (+ level-bugs (middle-bug outer-grid 'R))]
           [(= y (sub1 height))
            (+ level-bugs (middle-bug outer-grid 'D))]))]
      [(and (inner-edge? coord) (< level (sub1 (length grids))))
       (let* ([inner-grid (list-ref grids (add1 level))])
         (cond
           [(and (= x mid-x) (< y mid-y))
            (+ level-bugs (edge-bugs inner-grid 'U))]
           [(and (= x mid-x) (> y mid-y))
            (+ level-bugs (edge-bugs inner-grid 'D))]
           [(and (= y mid-y) (< x mid-x))
            (+ level-bugs (edge-bugs inner-grid 'L))]
           [(and (= y mid-y) (> x mid-x))
            (+ level-bugs (edge-bugs inner-grid 'R))]
           [else 0]))]
      [else level-bugs])))

;; step-tile : char -> number -> char
;; A bug dies unless it has exactly one bug neighbour
;; A bug spawns if a space has one or two bug neighbours
(define (step-tile char bugs)
  (match char
    [#\. #:when (<= 1 bugs 2) #\#]
    [#\# #:when (!= 1 bugs)   #\.]
    [else char]))

;; step : (coord -> number) -> grid -> grid
;; Return the grid after one minute of bugs dying and spawning
;; The bug-counter takes a coordinate and returns the number of
;; bugs adjacent to that coordinate
;; For part 1, this is simply adjacent-bugs
;; For part 2, we need to take into account previous and next levels,
;; and so should be adjacent-bugs* wrt to the current level and grids
(define (step bug-counter grid)
  (let ([new-grid (make-empty-grid)])
    (for ([coord (cartesian-product (range 0 width) (range 0 height))])
      (match-let* ([(list x y) coord]
                   [char (vector-ref (vector-ref grid y) x)]
                   [bugs (bug-counter coord)]
                   [next-char (step-tile char bugs)])
        (vector-set! (vector-ref new-grid y) x next-char)))
    new-grid))

;; step* : grids -> grids
;; Return the grids after one minute of bugs dying and spawning
;; The bugs on the edge and near the centre of a given level
;; will depend on the bugs near the centre and on the edge
;; of the previous and next levels
;; If the first (outermost) grid has bugs on the edge, their fate
;; will depend on a yet outer grid, so we prepend an empty grid
;; If the last (innermost) grid has bugs near the centre, their fate
;; will depend on a yet inner grid, so we postpend an empty grid
(define (step* grids)
  (let* ([grids*
          (foldr
           (λ (level grid grids*)
             (cons (step (∂ adjacent-bugs* level grids) grid) grids*))
           '() (range 0 (length grids)) grids)]
         [grids* (if (edges-bugs? (first grids*))
                     (cons (make-empty-grid) grids*)
                     grids*)]
         [grids* (if (middle-bugs? (last grids*))
                     (append grids* (list (make-empty-grid)))
                     grids*)])
    grids*))

;; biodiversity : grid -> number
;; From left to right and top to bottom, the biodiversity of the grid
;; is 2 to the power of the index of the tile if they were lined up
;; in order starting from an index of 0
(define (biodiversity grid)
  (let ([list-grid (append* (map vector->list (vector->list grid)))])
    (sum (map (λ (char i)
                (if (char=? char #\#)
                    (expt 2 i) 0))
              list-grid (range 0 (length list-grid))))))

;; total-bugs : grids -> number
(define (total-bugs grids)
  (sum (map (λ (grid)
              (sum (vector->list (vector-map (∂ vector-count (∂ char=? #\#)) grid))))
            grids)))

;; show-grids : grids -> void
;; Print each grid from outermost to innermost separated by line breaks
;; for... debugging
(define (show-grids grids)
  (for ([grid grids])
    (for ([row grid])
      (displayln (list->string (vector->list row))))
    (displayln "")))

(define part1
  (let loop ([grid vector-grid]
             [set (set vector-grid)])
    (let ([next-grid (step (∂ adjacent-bugs grid) grid)])
      (if (set-member? set next-grid)
          (biodiversity next-grid)
          (loop next-grid (set-add set next-grid))))))

(define part2
  (let loop ([count 0]
             [grids (list (make-empty-grid) vector-grid (make-empty-grid))])
    (if (= count 200)
        (total-bugs grids)
        (loop (add1 count) (step* grids)))))

(show-solution part1 part2)