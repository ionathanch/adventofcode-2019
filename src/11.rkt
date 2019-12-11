#lang plai

(require "../lib.rkt"
         "IntCode.rkt")

(define input
  (string->program (car (problem-input 11))))

;; The hull as a hash (x . y) -> c
;; x increases rightward
;; y increases downward
(define hull (make-hash))

;; The hull as a vector of row vectors of c
;; i.e. x = index of inner vectors,
;;      y = index of outer vector
(define (make-grid xrange yrange)
  (build-vector
   (add1 yrange)
   (λ (_) (make-vector (add1 xrange) #\ ))))

;; Turn CCW if δ = 0, CW if δ = 1
(define (update-dir dir δ)
  (match dir
    ['U (if (zero? δ) 'L 'R)]
    ['D (if (zero? δ) 'R 'L)]
    ['L (if (zero? δ) 'D 'U)]
    ['R (if (zero? δ) 'U 'D)]))

;; Move in given direction (up/down/left/right)
(define (update-pos dir xy)
  (let ([x (car xy)]
        [y (cdr xy)])
    (match dir
      ['U (cons x (sub1 y))]
      ['D (cons x (add1 y))]
      ['L (cons (sub1 x) y)]
      ['R (cons (add1 x) y)])))

;; Deploy the emergency hull painting robot!
(define (deploy st dir xy)
  (define clr (hash-ref hull xy 0))
  (type-case state st
    [halt (_) (void)]
    [in (resume)
        (let*-values
            ([(clr* st) (resume-with-output (resume clr))]
             [(δ st)    (resume-with-output st)])
          (cond [(neq? clr clr*)
                 (hash-set! hull xy clr*)])
          (let* ([dir (update-dir dir δ)]
                 [xy  (update-pos dir xy)])
            (deploy st dir xy)))]
    [else (error "Unexpected program state.")]))

(define (part1)
  (deploy (exec input) 'U '(0 . 0))
  (hash-count hull))

(define (part2)
  (set! hull (make-hash '(((0 . 0) . 1))))
  (deploy (exec input) 'U '(0 . 0))
  (let* ([positions (hash-keys hull)]
         [xs (map car positions)]
         [ys (map cdr positions)]
         [xmin (apply min xs)]  [xmax (apply max xs)]
         [ymin (apply min ys)]  [ymax (apply max ys)]
         [xrange (- xmax xmin)] [yrange (- ymax ymin)]
         [grid (make-grid xrange yrange)])
    (for-each
     (λ (kv)
       (let* ([x (- (caar kv) xmin)]
              [y (- (cdar kv) ymin)]
              [c (cdr kv)]
              [row (vector-ref grid y)])
         (cond [(= c 1) (vector-set! row x #\█)])))
     (hash->list hull))
    (for-each
     (λ (row) (displayln (list->string (vector->list row))))
     (vector->list grid))))

(show-solution (part1) (part2))