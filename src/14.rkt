#lang racket

(require graph
         (except-in "../lib.rkt" transpose))

(define input
  (problem-input 14))

(struct chemical (name amount) #:transparent)

;; deps : directed, unweighted graph from products to reactants
(define deps (unweighted-graph/directed '()))

;; eqs : hashtable from products to (number . (list chemical))
(define eqs (make-hash))

;; parse-equation : string -> void
;; Parses a string of the form "amt1 r1, ..., amtn rn => amtp p"
;; and adds the equation to both deps and eqs
(define (parse-equation! str)
  (let* ([equation (string-split str " => ")]
         [reactants (map (λ (s)
                           (let ([ss (string-split s " ")])
                             (chemical (string->symbol (second ss))
                                       (string->number (first ss)))))
                         (string-split (first equation) ", "))]
         [product (string-split (second equation) " ")]
         [product (chemical (string->symbol (second product))
                            (string->number (first product)))])
    (for-each
     (λ (reactant)
       (add-directed-edge! deps (chemical-name product) (chemical-name reactant)))
     reactants)
    (hash-set! eqs (chemical-name product) (cons (chemical-amount product) reactants))))

;; add-reactants : product -> (overflow . chems) -> (overflow . chems)
;; Given a product we want to produce, the amount we want is in chem;
;; add the required reactants and amounts to chems while using as many
;; overflow chemicals from previous loops as possible, and adding
;; leftover products from reactions into the overflow
(define (add-reactants product overflow-chems)
  (define overflow (car overflow-chems))
  (define chems (cdr overflow-chems))
  (if (hash-has-key? chems product)
      (let* ([have (hash-ref chems product)]
             [eq (hash-ref eqs product)]
             [need (car eq)]
             [reactants (cdr eq)]
             [multiple (ceiling (/ have need))]
             [remaining (- (* multiple need) have)]
             [overflow (hash-update overflow product (∂ + remaining) 0)]
             [chems (hash-remove chems product)])
        (foldl (λ (reactant oc)
                 (let* ([overflow (car oc)]
                        [chems (cdr oc)]
                        [name (chemical-name reactant)]
                        [need (* multiple (chemical-amount reactant))]
                        [have (hash-ref overflow name 0)]
                        [actual (pos-or-zero (- need have))]
                        [overflow (hash-update overflow name (∂ - (- need actual)) 0)]
                        [chems (hash-update chems name (∂ + actual) 0)])
                   (cons overflow chems)))
               (cons overflow chems) reactants))
      overflow-chems))

;; ore-needed : (listof products) -> (overflow . chems) -> number
;; overflow : product => number
;; chems : product => number
;; Calculates the amount of ORE needed to produce the given list of products
;; and the given list of available overflow reactants
(define (ore-needed sorted overflow-chems)
  (define overflow (car overflow-chems))
  (define chems (cdr overflow-chems))
  (if (and (= 1 (hash-count chems))
           (hash-has-key? chems 'ORE))
      (hash-ref chems 'ORE)
      (let* ([overflow-chems (foldl add-reactants overflow-chems sorted)])
        (ore-needed sorted overflow-chems))))

(define toposorted
  (begin
    (for-each parse-equation! input)
    (remove 'ORE (tsort deps))))

(define part1
  (ore-needed toposorted (cons (make-immutable-hash '()) (make-immutable-hash '((FUEL . 1))))))

;; Binary search using the ore amount from part 1 to get a fuel lower bound
;; (i.e. overestimating ore needed) and twice of that as an upper bound
(define part2
  (let loop ([lower (floor (/ 1000000000000 part1))]
             [upper (* 2 (floor (/ 1000000000000 part1)))])
    (if (= 1 (- upper lower)) lower
        (let* ([mid (+ (floor (/ (- upper lower) 2)) lower)]
               [ore (ore-needed toposorted
                                (cons (make-immutable-hash '())
                                      (make-immutable-hash `((FUEL . ,mid)))))])
          (if (> ore 1000000000000)
              (loop lower mid)
              (loop mid upper))))))

(show-solution part1 part2)