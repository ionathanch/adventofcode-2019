#lang racket

(require graph
         data/heap
         racket/set
         (except-in "../lib.rkt" transpose))

(define input
  (problem-input 20))

(define list-grid
  (map string->list input))

(define width
  (length (first list-grid)))
(define height
  (length list-grid))

(define vector-grid
  (lists->vectors list-grid))

;; get-char : coord -> char
;; coord = (list number number)
;; Get the character at the specified coordinate
;; where x increases rightwards and y increases downwards.
(define (get-char coord)
  (match-let ([(list x y) coord])
    (vector-ref (vector-ref vector-grid y) x)))

;; neighbours : coord -> (listof coord)
;; If the coordinate is an open passage,
;; get the passages accessible from it;
;; otherwise, return an empty list.
(define (neighbours coord)
  (if (nchar=? #\. (get-char coord)) '()
      (match-let* ([(list x y) coord]
                   [ncoords (list (list (add1 x) y)
                                  (list (sub1 x) y)
                                  (list x (add1 y))
                                  (list x (sub1 y)))])
        (filter (∘ (∂ char=? #\.) (∂ get-char)) ncoords))))

;; graph-grid : unweighted, undirected graph
;; Vertices: coords
;; Edges: between accessible passages
(define graph-grid
  (let ([graph (unweighted-graph/undirected '())]
        [coords (cartesian-product (range 2 (- width 2))
                                   (range 2 (- height 2)))])
    (for ([coord coords])
      (for ([ncoord (neighbours coord)])
        (add-edge! graph coord ncoord)))
    graph))

;; add-coord-pairs! : (hashof (symbol => coord)) -> (listof (list coord coord))
;;                                              -> ('top | 'bottom | 'left | 'right)
;;                                              -> void
;; The coordinate pairs represent the stripes of points bordering the maze.
;; For instance, the top stripe would be the list (((0 0) (0 1)) (((1 0) (1 1)) ...)
;; We check whether there are two letters at those two points, which represent a portal.
;; Then we add the portal into the hash from the symbol of its name to its coordinate.
;; The side parameter tells us if we're looking at a stripe that borders the maze on the
;; top, bottom, left, or right. Notice that for the inner portals, the "top" stripe
;; (i.e. with smallest y coordinates) is a 'bottom stripe, since it sits below a row of maze.
(define (add-coord-pairs! hash coord-pairs side)
  (for ([coord-pair coord-pairs])
    (match-let* ([(list c1 c2) coord-pair]
                 [char1 (get-char c1)]
                 [char2 (get-char c2)]
                 [(list x y) (match side
                               [(or 'bottom 'right) c1]
                               [(or 'top 'left) c2])]
                 [coord (match side
                          ['top    (list x (add1 y))]
                          ['bottom (list x (sub1 y))]
                          ['left   (list (add1 x) y)]
                          ['right  (list (sub1 x) y)])])
      (when (and (not (member char1 '(#\# #\. #\ )))
                 (not (member char2 '(#\# #\. #\ ))))
        (hash-set! hash
                   (string->symbol (list->string (list char1 char2)))
                   coord)))))

;; make-portals : (list number number) x 4 -> (hashof (symbol => coord))
;; For each argument, the first number in the list represents the coordinate
;; of the first letter of the portals in that stripe, and idem for the second.
;; The coordinate given depends on whether the stripe is vertical or horizontal.
;; For instance, for the outer portals, the top stripe runs horizontally,
;; so x varies over all possible points, while y = 0, 1.
;; For the right stripe, which runs vertically, x = width - 2, width - 1.
;; The order of the coordinates gives the order of the letters of the name of the portal.
;; This returns a hashmap from portal symbol names to their coordinates.
(define (make-portals top-coords bottom-coords left-coords right-coords)
  (let ([hash (make-hash)]
        [top-coord-pairs
         (map list
              (cartesian-product (range 0 width) (list (first  top-coords)))
              (cartesian-product (range 0 width) (list (second top-coords))))]
        [bottom-coord-pairs
         (map list
              (cartesian-product (range 0 width) (list (first  bottom-coords)))
              (cartesian-product (range 0 width) (list (second bottom-coords))))]
        [left-coord-pairs
         (map list
              (cartesian-product (list (first  left-coords)) (range 0 height))
              (cartesian-product (list (second left-coords)) (range 0 height)))]
        [right-coord-pairs
         (map list
              (cartesian-product (list (first  right-coords)) (range 0 height))
              (cartesian-product (list (second right-coords)) (range 0 height)))])
    (add-coord-pairs! hash top-coord-pairs    'top)
    (add-coord-pairs! hash bottom-coord-pairs 'bottom)
    (add-coord-pairs! hash left-coord-pairs   'left)
    (add-coord-pairs! hash right-coord-pairs  'right)
    hash))

;; outer-portals : (hashof (symbol => coord))
;; A list of the portals on the outer edge of the maze.
;; Note that this also includes the start and end points AA, ZZ,
;; so it should have two more entries than inner-portals.
(define outer-portals
  (make-portals (list 0 1)
                (list (- height 2) (- height 1))
                (list 0 1)
                (list (- width 2)  (- width 1))))

;; inner-portals : (hashof (symbol => coord))
;; A list of the portals on the inner edge of the maze.
(define inner-portals
  (make-portals (list 90 91)
                (list 37 38)
                (list 96 97)
                (list 37 38)))

;; Since we can travel between portals of the same name,
;; we add an edge between them in the graph.
(for ([portal (hash-keys inner-portals)])
  (add-edge! graph-grid (hash-ref outer-portals portal) (hash-ref inner-portals portal)))

(define part1
  (sub1 (length (fewest-vertices-path graph-grid
                                      (hash-ref outer-portals 'AA)
                                      (hash-ref outer-portals 'ZZ)))))

;; This is a BFS of the graph, taking into account the level.
;; Heap elements are (list coord number (setof (list coord number)) number)
;; or, giving them names, (list coord level visited count), where
;; - coord is the current position
;; - level is the current floor level
;; - visited is a set of visited locations consisting of a position and a level
;; - count is the number of steps we have taken since AA
;; When visiting a position + level, we consider its neighbours to visit next.
;; If we are travelling from an inner portal to an outer portal, we are going
;; one level deeper, so we increment the level, and vice versa for outer to inner.
;; We do not allow negative levels, and we do not revisit locations.
;; We also do not visit any level deeper than 25; this was found through trial and error.
;; If we are at ZZ on level 0, we're done, and we return the number of steps taken.
;; If we run out of locations to visit (e.g. by restricting the deepest level to < 25),
;; we will get an empty-heap error.
(define part2
  (let* ([inner-portal-coords (list->set (hash-values inner-portals))]
         [outer-portal-coords (list->set (hash-values outer-portals))]
         [heap (make-heap (λ (v1 v2) (< (fourth v1) (fourth v2))))]
         [AA (hash-ref outer-portals 'AA)]
         [ZZ (hash-ref outer-portals 'ZZ)])
    (heap-add! heap (list AA 0 (set (cons AA 0)) 0))
    (match-let loop ([(list coord level visited count) (heap-min heap)])
      (heap-remove-min! heap)
      (if (and (= level 0) (equal? coord ZZ))
          count
          (begin
            (for ([ncoord (get-neighbors graph-grid coord)])
              (let* ([level
                      (cond
                        [(and (set-member? inner-portal-coords coord)
                              (set-member? outer-portal-coords ncoord)) (add1 level)]
                        [(and (set-member? outer-portal-coords coord)
                              (set-member? inner-portal-coords ncoord)) (sub1 level)]
                        [else level])])
                (unless (or (< level 0) (> level 25) (set-member? visited (cons ncoord level)))
                  (heap-add! heap (list ncoord
                                        level
                                        (set-add visited (cons ncoord level))
                                        (add1 count))))))
            (loop (heap-min heap)))))))

(show-solution part1 part2)