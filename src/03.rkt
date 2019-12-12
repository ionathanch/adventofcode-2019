#lang racket

(require "../lib.rkt")

;; wire : (listof path)
;; path : (symbol . number)
(define-values (wire1 wire2)
  (let* ([input (problem-input 3)]
         [string->path (λ (str) (cons (string->symbol (substring str 0 1))
                                      (string->number (substring str 1))))]
         [string->wire (λ (str) (map string->path (string-split str ",")))]
         [wire1 (string->wire (first input))]
         [wire2 (string->wire (second input))])
    (values wire1 wire2)))

;; To store the paths of each wire, we will use a hashtable.
;; The keys of the hashtable are positions on the grid,
;;   stored as a pair of numbers (x . y).
;; The values of the hashtable are points on the grid,
;;   which contain four pieces of information:
;;   - first-visited? : Whether the first wire has traversed the point;
;;   - first-steps : How many steps from the origin the first wire has taken;
;;   - second-visited?, second-steps: idem but for the second wire.
(define hashtable (make-hash))

(struct point
  (first-visited?
   first-steps
   second-visited?
   second-steps)
  #:transparent)
(define default-point (point #f 0 #f 0))

(define origin (cons 0 0))
(define (manhattan xy)
  (+ (abs (car xy))
     (abs (cdr xy))))

(define (update! first-wire? x y count)
  (hash-update!
   hashtable (cons x y)
   (λ (pt)
     (if first-wire?
         (struct-copy
          point pt
          [first-visited? #t]
          [first-steps count])
         (struct-copy
          point pt
          [second-visited? #t]
          [second-steps count])))
   default-point)
  (add1 count))

;; step-path : boolean -> path -> (pos . number) -> (pos . number)
;;   first-wire? : Indicates whether we're stepping for the first or second wire.
;;   path : A pair (s: symbol . n: number), where s indicates the direction we step
;;     (R, L, U, D for right (+x), left (-x), up (+y), down (-y))
;;     and n indicates the number of units we step in that direction
;;   pos-count : A pair (pos . number) that indicates the current location and the steps taken
;;   hashtable : A map from pos to point
;; Returns the new position after stepping
(define (step-path! first-wire? path pos-count)
  (let* ([x (caar pos-count)]
         [y (cdar pos-count)]
         [count (cdr pos-count)]
         [n (cdr path)])
    (match (car path)
      ['R
       (cons (cons (+ x n) y)
             (foldl (∂ update! first-wire?) count
                    (range (add1 x) (add1 (+ x n)))
                    (make-list n y)))]
      ['L
       (cons (cons (- x n) y)
             (foldl (∂ update! first-wire?) count
                    (reverse (range (- x n) x))
                    (make-list n y)))]
      ['U
       (cons (cons x (+ y n))
             (foldl (∂ update! first-wire?) count
                    (make-list n x)
                    (range (add1 y) (add1 (+ y n)))))]
      ['D
       (cons (cons x (- y n))
             (foldl (∂ update! first-wire?) count
                    (make-list n x)
                    (reverse (range (- y n) y))))])))

(define (step-wire! first-wire? wire)
  (foldl (∂ step-path! first-wire?) `(,origin . 1) wire))

(define (intersect ht)
  (filter-map
   (λ (xypt)
     (define pt (cdr xypt))
     (and (point-first-visited? pt)
          (point-second-visited? pt)
          (cons (car xypt)
                (+ (point-first-steps pt)
                   (point-second-steps pt)))))
   (hash->list ht)))

(define-values (part1 part2)
  (let* ([_ (step-wire! #t wire1)]
         [_ (step-wire! #f wire2)]
         [intersections (intersect hashtable)]
         [distances (map (∘ manhattan car) intersections)]
         [steps (map cdr intersections)])
    (values (apply min distances)
            (apply min steps))))

(show-solution part1 part2)