#lang racket

(require racket/set
         data/heap
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
  (cons #\@ (list (round (/ width 2))
                            (round (/ height 2)))))

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
;; Vertices are (char . coord)
;; Edges between traversable coordinates
(define graph-grid
  (let* ([coords (cartesian-product (range 1 (sub1 width))
                                    (range 1 (sub1 height)))]
         [graph (unweighted-graph/undirected '())])
    (for ([coord coords])
      (let ([ncoords (neighbours coord)])
        (for ([ncoord ncoords])
          (add-edge! graph
                     (cons (get-char  coord) coord)
                     (cons (get-char ncoord) ncoord)))))
    graph))

;; keys-hash : (hashof (char => coord))
;; A hashmap from keys to their coordinates, including #\@
(define keys-hash
  (let ([hash (make-immutable-hash)]
        [coords (cartesian-product (range 1 (sub1 width))
                                   (range 1 (sub1 height)))])
    (foldl (λ (coord hash)
             (let ([char (get-char coord)])
               (if (or (char=? #\@ char)
                       (char-lower-case? char))
                   (hash-set hash char coord)
                   hash)))
           hash coords)))

;; inter-keys-hash : (hashof (char => char))
;; A hashmap from keys to the keys that must be collected
;; when taking the shortest path from #\@ to that key
(define inter-keys-hash
  (let ([hash (make-immutable-hash)])
    (foldl (λ (keycoord hash)
             (match-let* ([(cons key _) keycoord]
                          [path (map car (fewest-vertices-path graph-grid start keycoord))]
                          [inter-keys (remove* (list #\@ key) (filter char-lower-case? path))])
               (hash-set hash key inter-keys)))
           hash (hash->list keys-hash))))

;; doors : (hashof (char => (listof char)))
;; A hashmap from keys to the list of keys for the doors
;; that stand between the starting point #\@ and that key
(define doors-hash
  (let ([hash (make-immutable-hash)])
    (foldl (λ (keycoord hash)
             (match-let* ([(cons key _) keycoord]
                          [path (map car (fewest-vertices-path graph-grid start keycoord))]
                          [doors (map char-downcase (filter char-upper-case? path))])
               (hash-set hash key doors)))
           hash (hash->list keys-hash))))

;; key-graph : weighted, undirected graph
;; Vertices are char (keys)
;; Edges between neighbouring keys
;; Weights are distances between keys
(define key-graph
  (let ([graph (weighted-graph/undirected '())]
        [key-pairs (combinations (hash-keys keys-hash) 2)])
    (for ([pair key-pairs])
      (match-let* ([(list key1 key2) pair]
                   [keycoord1 (cons key1 (hash-ref keys-hash key1))]
                   [keycoord2 (cons key2 (hash-ref keys-hash key2))]
                   [path (fewest-vertices-path graph-grid keycoord1 keycoord2)]
                   [distance (sub1 (length path))])
        (add-edge! graph key1 key2 distance)))
    graph))

;; visitable? : (setof char) -> char -> boolean
;; Given a set of visited keys and a prospective key,
;; return whether we visit that key based on three conditions:
;; - There isn't a closer key we could visit;
;; - The key has not yet been visited; and
;; - We have the keys needed to open all doors leading to that key.
(define (visitable? visited key)
  (let* ([visitable-inter-keys (filter-not (∂ set-member? visited) (hash-ref inter-keys-hash key))])
    (and (empty? visitable-inter-keys)
         (not (set-member? visited key))
         (andmap (∂ set-member? visited) (hash-ref doors-hash key)))))

(struct state (key visited steps) #:transparent)

(define (state=? st1 st2)
  (and (equal? (state-key st1) (state-key st2))
       (equal? (state-visited st1) (state-visited st2))))

(define (state<? st1 st2)
  (< (state-steps st1) (state-steps st2)))

(define part1
  (let ([heap (make-heap state<?)]
        [memo (make-hash)])
    (heap-add! heap (state #\@ (set #\@) 0))
    (match-let loop ([(state key visited steps) (heap-min heap)])
      (heap-remove-min! heap)
      (if (= (set-count visited) (hash-count keys-hash))
          steps
          (let* ([visitable (filter (∂ visitable? visited) (get-neighbors key-graph key))])
            (for ([nkey visitable])
              (let* ([visited (set-add visited nkey)]
                     [steps (+ steps (edge-weight key-graph key nkey))]
                     [memo-steps (hash-ref memo (cons nkey visited) +inf.0)]
                     [st (state nkey visited steps)])
                (when (< steps memo-steps)
                  (hash-set! memo (cons nkey visited) steps)
                  (heap-remove! heap st #:same? state=?)
                  (heap-add! heap st))))
            (loop (heap-min heap)))))))

#;(for-each displayln (map (λ (s) (string-replace (string-replace s "#" "█") "." " ")) input))