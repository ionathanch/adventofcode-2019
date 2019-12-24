#lang racket

(require racket/set
         "../lib.rkt")

(define input
  (problem-input 24))

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

(define (make-empty-grid)
  (build-vector height (λ (_) (make-vector width #\.))))

(define (outer-edge? coord)
  (match-let ([(list x y) coord])
    (or (= x 0) (= y 0)
        (= x (sub1 width)) (= y (sub1 height)))))

(define (inner-edge? coord)
  (match-let ([(list x y) coord])
    (or (= x mid-x)
        (= y mid-y))))

(define (edge-bugs grid side)
  (match side
    ['U (vector-count (∂ char=? #\#) (vector-ref grid 0))]
    ['D (vector-count (∂ char=? #\#) (vector-ref grid (sub1 height)))]
    ['L (vector-count (∂ char=? #\#) (vector-map (∂ vector-first) grid))]
    ['R (vector-count (∂ char=? #\#) (vector-map (∂ vector-last)  grid))]))

(define (edges-bugs? grid)
  (!= 0 (+ (edge-bugs grid 'U)
           (edge-bugs grid 'D)
           (edge-bugs grid 'L)
           (edge-bugs grid 'R))))

(define (middle-bug grid side)
  (match side
    ['U (if (char=? #\# (vector-grid-ref* grid (list mid-x (sub1 mid-y)) #\.)) 1 0)]
    ['D (if (char=? #\# (vector-grid-ref* grid (list mid-x (add1 mid-y)) #\.)) 1 0)]
    ['L (if (char=? #\# (vector-grid-ref* grid (list (sub1 mid-x) mid-y) #\.)) 1 0)]
    ['R (if (char=? #\# (vector-grid-ref* grid (list (add1 mid-x) mid-y) #\.)) 1 0)]))

(define (middle-bugs? grid)
  (!= 0 (+ (middle-bug grid 'U)
           (middle-bug grid 'D)
           (middle-bug grid 'L)
           (middle-bug grid 'R))))

(define (adjacent-bugs grid coord)
  (match-let* ([(list x y) coord]
               [U (vector-grid-ref* grid (list x (sub1 y)) #\.)]
               [D (vector-grid-ref* grid (list x (add1 y)) #\.)]
               [L (vector-grid-ref* grid (list (sub1 x) y) #\.)]
               [R (vector-grid-ref* grid (list (add1 x) y) #\.)])
    (count (∂ char=? #\#) (list U D L R))))

(define (adjacent-bugs* grids level coord)
  (match-let* ([(list x y) coord]
               [grid (list-ref grids level)]
               [level-bugs (adjacent-bugs grid coord)])
    (cond
      [(and (outer-edge? coord) (> level 0))
       (let* ([outer-grid (list-ref grids (sub1 level))])
         (cond
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
           [else level-bugs]))]
      [else level-bugs])))

(define (step grid)
  (let ([new-grid (make-empty-grid)])
    (for ([coord (cartesian-product (range 0 width) (range 0 height))])
      (match-let* ([(list x y) coord]
                   [char (vector-ref (vector-ref grid y) x)]
                   [bug-count (adjacent-bugs grid coord)]
                   [next-char
                    (match char
                      [#\. #:when (<= 1 bug-count 2) #\#]
                      [#\# #:when (!= 1 bug-count) #\.]
                      [else char])])
        (vector-set! (vector-ref new-grid y) x next-char)))
    new-grid))

(define (step* grids)
  (let* ([grids*
          (foldl
           (λ (level grid grids*)
             (let ([new-grid (make-empty-grid)])
               (for ([coord (cartesian-product (range 0 width) (range 0 height))])
                 (match-let* ([(list x y) coord]
                              [char (vector-ref (vector-ref grid y) x)]
                              [bug-count (adjacent-bugs* grids level coord)]
                              [next-char
                               (match char
                                 [#\. #:when (<= 1 bug-count 2) #\#]
                                 [#\# #:when (!= 1 bug-count) #\.]
                                 [else char])])
                   (vector-set! (vector-ref new-grid y) x next-char)))
               (cons new-grid grids*)))
           '() (range 0 (length grids)) grids)]
         [grids* (if (edges-bugs? (first grids*))
                     (cons (make-empty-grid) grids*)
                     grids*)]
         [grids* (if (middle-bugs? (last grids*))
                     (append grids* (list (make-empty-grid)))
                     grids*)])
    grids*))

(define (biodiversity grid)
  (let ([list-grid (append* (map vector->list (vector->list grid)))])
    (sum (map (λ (char i)
                (if (char=? char #\#)
                    (expt 2 i) 0))
              list-grid (range 0 (length list-grid))))))

(define (total-bugs grids)
  (sum (map (λ (grid)
              (sum (vector->list (vector-map (∂ vector-count (∂ char=? #\#)) grid))))
            grids)))

(define part1
  (let loop ([grid vector-grid]
             [set (set vector-grid)])
    (let ([next-grid (step grid)])
      (if (set-member? set next-grid)
          (biodiversity next-grid)
          (loop next-grid (set-add set next-grid))))))

(define part2
  (let loop ([count 0]
             [grids (list (make-empty-grid) vector-grid (make-empty-grid))])
    (if (= count 10)
        (total-bugs grids)
        (loop (add1 count) (step* grids)))))

(show-solution part1 part2)