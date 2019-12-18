#lang racket

(require data/queue
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
  
(define (get-char coord)
  (match-let ([(list x y) coord])
    (if (or (< x 0) (< y 0)
            (>= x width)
            (>= y height))
        #\#
        (vector-ref (vector-ref vector-grid y) x))))

(define (neighbours coord)
  (match-let ([(list x y) coord])
    (let ([U (list x (sub1 y))]
          [D (list x (add1 y))]
          [L (list (sub1 x) y)]
          [R (list (add1 x) y)])
      (filter-not
       (∘ (∂ char=? #\#) (∂ get-char))
       (list U D L R)))))

(define key-door-graph
  (let* ([Q (make-queue)]
         [entrance (list (round (/ width 2))
                         (round (/ height 2)))]
         [visited (make-hash `((,entrance . #t)))]
         [graph (weighted-graph/undirected '())])
    (enqueue! Q (list entrance #\@ 0))
    (let loop ()
      (if (queue-empty? Q)
          graph
          (match-let* ([(list coord prev dist) (dequeue! Q)]
                       [ncoords (neighbours coord)])
            (for-each
             (λ (ncoord)
               (let ([nc (get-char ncoord)])
                 (cond
                   [(hash-ref visited ncoord #f)]
                   [(char=? #\. nc)
                    (hash-set! visited ncoord #t)
                    (enqueue! Q (list ncoord prev (add1 dist)))]
                   [else
                    (hash-set! visited ncoord #t)
                    (enqueue! Q (list ncoord nc 0))
                    (add-edge! graph prev nc (add1 dist))])))
             ncoords)
            (loop))))))

(define ordered-keys
  (let ([keys (filter char-lower-case? (get-vertices key-door-graph))])
    (displayln keys)
    (define (sort-fn c1 c2)
      (not (member (char-upcase c2) (fewest-vertices-path key-door-graph #\@ c1))))
    (sort keys sort-fn)))

#;(for-each displayln (map (λ (s) (string-replace (string-replace s "#" "█") "." " ")) input))