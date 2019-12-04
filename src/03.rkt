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

;; step-path : boolean -> path -> (number . number) -> hashtable -> procedure -> (number . number)
;;   first-wire? : Indicates whether we're stepping for the first or second wire.
;;   path : A pair (s: symbol . n: number), where s indicates the direction we step
;;     (R, L, U, D for right (+x), left (-x), up (+y), down (-y))
;;     and n indicates the number of units we step in that direction
;;   position : A pair (x . y) that indicates the current location
;;   hashtable : A map from positions to ((boolean . number) . (boolean . number))that indicate
;;     the total number of steps to get to that position and
;;     whether the first or second wire has visited that position
;; Returns the new position after stepping
(define (step-path first-wire? path pos-count ht)
  (let* ([position (car pos-count)]
         [count    (cdr pos-count)]
         [x (car position)]
         [y (cdr position)]
         [update (λ (dir)
                   (λ (i count)
                     (let* ([key (match dir
                                   ['x (cons i y)]
                                   ['y (cons x i)])]
                            [value (hash-ref ht key '((#f . 0) . (#f . 0)))]
                            [new-count (add1 count)]
                            [new-value
                             (if first-wire?
                                 (cons `(#t . ,new-count) (cdr value))
                                 (cons (car value) `(#t . ,new-count)))])
                       (hash-set! ht key new-value)
                       new-count)))])
    (match path
      [`(R . ,(? number? n))
       (cons (cons (+ x n) y)
             (foldl (update 'x) count (range (add1 x) (add1 (+ x n)))))]
      [`(L . ,(? number? n))
       (cons (cons (- x n) y)
             (foldl (update 'x) count (reverse (range (- x n) x))))]
      [`(U . ,(? number? n))
       (cons (cons x (+ y n))
             (foldl (update 'y) count (range (add1 y) (add1 (+ y n)))))]
      [`(D . ,(? number? n))
       (cons (cons x (- y n))
             (foldl (update 'y) count (reverse (range (- y n) y))))])))

(define (step-wire first-wire? wire ht)
  (foldl (λ (path pos-count) (step-path first-wire? path pos-count ht)) '((0 . 0) . 0) wire))

(define-values (part1 intersections)
  (let* ([hashtable (make-hash '(((0 . 0) . ((#f . 0) . (#f . 0)))))]
         [_ (step-wire #t wire1 hashtable)]
         [_ (step-wire #f wire2 hashtable)]
         [hashlist (hash->list hashtable)]
         [intersections
          (filter (λ (kv)
                    (let ([v (cdr kv)])
                      (and (caar v) (cadr v))))
                  hashlist)]
         [distances
          (map (λ (kv)
                 (let ([k (car kv)])
                   (+ (abs (car k)) (abs (cdr k)))))
               intersections)])
    (values (apply min distances)
            intersections)))

(define part2
  (let* ([steps
          (map (λ (kv)
                 (let ([v (cdr kv)])
                   (+ (cdar v) (cddr v))))
               intersections)])
    (apply min steps)))

(show-solution part1 part2)