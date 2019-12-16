#lang racket

(require "../lib.rkt")

(define-values (start end) (values 235741 706948))

(define (two-adjacent-digits? cs)
  (if (< (length cs) 2) #f
      (or (eq? (first cs)
               (second cs))
          (two-adjacent-digits? (cdr cs)))))

(define (digits-never-decrease? cs)
  (if (< (length cs) 2) #t
      (and (char<=? (first cs)
                    (second cs))
           (digits-never-decrease? (cdr cs)))))

(define (has-group-of-two? cs)
  (cond [(< (length cs) 2) #f]
        [(= (length cs) 2)
         (char=? (first cs) (second cs))]
        [else
         (or (and (char=? (first cs)
                          (second cs))
                  (nchar=? (second cs)
                           (third cs)))
             (has-group-of-two? (dropf cs (λ (c) (eq? c (first cs))))))]))

(define-values (part1 part2)
  (let* ([passwords (map (compose string->list number->string) (range start (add1 end)))]
         [passwords (filter two-adjacent-digits? passwords)]
         [passwords (filter digits-never-decrease? passwords)]
         [passwords* (filter has-group-of-two? passwords)])
    (values (length passwords)
            (length passwords*))))

(show-solution part1 part2)