#lang racket

(require data/heap
         graph
         (except-in "../lib.rkt" transpose))

(define input
  (problem-input 18))

(define list-grid
  (map string->list input))

(define vector-grid
  (lists->vectors list-grid))

(define width
  (length (car list-grid)))

(define height
  (length list-grid))

;; start : (coord . char)
;; coord = (list number number)
(define start
  (cons (list (round (/ width 2))
              (round (/ height 2)))
        #\@))

;; get-char : coord -> char
(define (get-char coord)
  (match-let ([(list x y) coord])
    (if (or (< x 0) (< y 0)
            (>= x width)
            (>= y height))
        #\#
        (vector-ref (vector-ref vector-grid y) x))))

;; neighbours : coord -> (listof coord)
;; If the coordinate is occupiable (by a key, door, or bot),
;; return the neighbouring coordinates that are also occupiable
(define (neighbours coord)
  (if (char=? #\# (get-char coord)) '()
      (match-let ([(list x y) coord])
        (let ([U (list x (sub1 y))]
              [D (list x (add1 y))]
              [L (list (sub1 x) y)]
              [R (list (add1 x) y)])
          (filter-not
           (∘ (∂ char=? #\#) (∂ get-char))
           (list U D L R))))))

;; graph-grid : unweighted, undirected graph
;; Vertices are (coord . char)
;; Edges between traversable coordinates
(define graph-grid
  (let* ([coords (cartesian-product (range 1 (sub1 width))
                                    (range 1 (sub1 height)))]
         [graph (unweighted-graph/undirected '())])
    (for-each
     (λ (coord)
       (let ([ncoords (neighbours coord)])
         (for-each
          (λ (ncoord)
            (add-edge! graph
                       (cons  coord (get-char  coord))
                       (cons ncoord (get-char ncoord))))
          ncoords)))
     coords)
    graph))

;; keys : (listof (coord . char))
;; Pairs of keys and their coordinates, including #\@
(define keys
  (let ([coords (cartesian-product (range 1 (sub1 width))
                                   (range 1 (sub1 height)))])
    (foldl (λ (coord keys)
             (let ([char (get-char coord)])
               (if (or (char=? #\@ char)
                       (char-lower-case? char))
                   (cons (cons coord char) keys)
                   keys)))
           '() coords)))

;; doors : (hashof (char => (listof char)))
;; A hashmap from keys to the list of keys for the doors
;; that stand between the starting point #\@ and that key
(define doors-hash
  (let ([hash (make-hash)])
    (for ([key keys])
      (let ([path (fewest-vertices-path graph-grid start key)])
        (hash-set! hash (cdr key)
                   (filter-map
                    (λ (v)
                      (if (char-upper-case? (cdr v))
                          (char-downcase (cdr v))
                          #f))
                    path))))
    hash))

;; key-graph : weighted, undirected graph
;; Vertices are char (keys)
;; Edges between neighbouring keys
;; Weights are distances between keys
(define key-graph
  (let ([graph (weighted-graph/undirected '())]
        [key-pairs (combinations keys 2)])
    (for ([pair key-pairs])
      (match-let* ([(list key1 key2) pair]
                   [path (fewest-vertices-path graph-grid key1 key2)]
                   [distance (sub1 (length path))])
        (add-edge! graph (cdr key1) (cdr key2) distance)))
    graph))

(define (search)
  (let ([heap (make-heap (λ (v1 v2) (< (last v1) (last v2))))])
    (heap-add! heap (list #\@ (make-immutable-hash '((#\@ . #t))) 0))
    (match-let loop ([(list key hash count) (heap-min heap)])
      (heap-remove-min! heap)
      (if (= (hash-count hash) (length keys))
          count
          (let* ([visitable
                  (filter (λ (nkey)
                            (and (not (hash-has-key? hash nkey))
                                 (andmap (∂ hash-has-key? hash) (hash-ref doors-hash nkey))))
                          (get-neighbors key-graph key))])
            (for ([nkey visitable])
              (heap-add! heap
                         (list nkey
                               (hash-set hash nkey #t)
                               (+ count (edge-weight key-graph key nkey)))))
            (loop (heap-min heap)))))))

#;(for-each displayln (map (λ (s) (string-replace (string-replace s "#" "█") "." " ")) input))