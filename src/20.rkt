#lang racket

(require graph
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

(define (get-char coord)
  (match-let ([(list x y) coord])
    (vector-ref (vector-ref vector-grid y) x)))

(define (neighbours coord)
  (if (nchar=? #\. (get-char coord)) '()
      (match-let* ([(list x y) coord]
                   [ncoords (list (list (add1 x) y)
                                  (list (sub1 x) y)
                                  (list x (add1 y))
                                  (list x (sub1 y)))])
        (filter (∘ (∂ char=? #\.) (∂ get-char)) ncoords))))

(define graph-grid
  (let ([graph (unweighted-graph/undirected '())]
        [coords (cartesian-product (range 2 (- width 2))
                                   (range 2 (- height 2)))])
    (for ([coord coords])
      (for ([ncoord (neighbours coord)])
        (add-edge! graph coord ncoord)))
    graph))

(define (add-coord-pairs hash coord-pairs side)
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
    (add-coord-pairs hash top-coord-pairs    'top)
    (add-coord-pairs hash bottom-coord-pairs 'bottom)
    (add-coord-pairs hash left-coord-pairs   'left)
    (add-coord-pairs hash right-coord-pairs  'right)
    hash))

(define outer-portals
  (make-portals (list 0 1)
                (list (- height 2) (- height 1))
                (list 0 1)
                (list (- width 2)  (- width 1))))

(define inner-portals
  (make-portals (list 90 91)
                (list 37 38)
                (list 96 97)
                (list 37 38)))

(for ([portal (hash-keys inner-portals)])
  (add-edge! graph-grid (hash-ref outer-portals portal) (hash-ref inner-portals portal)))

(define part1
  (sub1 (length (fewest-vertices-path graph-grid
                                      (hash-ref outer-portals 'AA)
                                      (hash-ref outer-portals 'ZZ)))))

(show-solution part1 #f)