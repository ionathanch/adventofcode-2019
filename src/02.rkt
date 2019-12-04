#lang racket

(require "../lib.rkt")

(define input
  (map string->number (string-split (car (problem-input 2)) ",")))

(define (input-nv nv)
  (append (list (car input) (first nv) (second nv)) (cdddr input)))

(define (exec pointer program)
  (let* ([opcode (list-ref program pointer)]
         [val1 (list-ref program (list-ref program (+ pointer 1)))]
         [val2 (list-ref program (list-ref program (+ pointer 2)))]
         [val3 (list-ref program (+ pointer 3))]
         [next-program
          (cond [(= opcode 1)
                 (list-set program val3 (+ val1 val2))]
                [(= opcode 2)
                 (list-set program val3 (* val1 val2))]
                [else program])])
    (if (= opcode 99)
        next-program
        (exec (+ pointer 4) next-program))))

(define part1
  (car (exec 0 (input-nv '(12 2)))))

(define part2
  (let* ([nounverbs (cartesian-product (range 100) (range 100))]
         [outputs (map (λ (nv) (car (exec 0 (input-nv nv)))) nounverbs)]
         [nounverb (list-ref nounverbs (index-of outputs 19690720))]
         [noun (first nounverb)]
         [verb (second nounverb)])
    (+ (* 100 noun) verb)))

(show-solution part1 part2)



;;;; ALTERNATE SOLUTION

(define part2-input
  (append (list (car input) 'noun 'verb '(+ noun verb)) (cddddr input)))

(define (exec-sym pointer program)
  (let* ([opcode (list-ref program pointer)]
         [val1 (list-ref program (list-ref program (+ pointer 1)))]
         [val2 (list-ref program (list-ref program (+ pointer 2)))]
         [val3 (list-ref program (+ pointer 3))]
         [next-program
          (cond [(= opcode 1)
                 (list-set program val3 `(+ ,val1 ,val2))]
                [(= opcode 2)
                 (list-set program val3 `(* ,val1 ,val2))]
                [else program])])
    (if (= opcode 99)
        next-program
        (exec-sym (+ pointer 4) next-program))))

(define part2-partial
  (car (exec-sym 4 part2-input)))

(define (simplify expr)
  (match expr
    ; (op n1 n2 e) -> (simplify (op (n1 `op` n2) (simplify e)))
    [(or `(,op (,op ,(? number? n1) ,e) ,(? number? n2))
         `(,op (,op ,e ,(? number? n1)) ,(? number? n2))
         `(,op ,(? number? n1) (,op ,(? number? n2) ,e))
         `(,op ,(? number? n1) (,op ,e ,(? number? n2))))
     (let ([opfun (match op ['+ +] ['* *])])
       (simplify `(,op ,(opfun n1 n2) ,(simplify e))))]

    ; (op e1 e2 s) -> (op (simplify (op e1 e2)) s)
    [(or `(,op (,op ,e1 ,(? symbol? s)) ,e2)
         `(,op (,op ,(? symbol? s) ,e1) ,e2)
         `(,op ,e1 (,op ,e2 ,(? symbol? s)))
         `(,op ,e1 (,op ,(? symbol? s) ,e2)))
     `(,op ,(simplify `(,op ,e1 ,e2)) ,s)]

    ; (* (+ n1 e) n2) -> (simplify (+ (n1 * n2) (simplify (* n1 e))))
    [(or `(* (+ ,(? number? n2) ,e) ,(? number? n1))
         `(* (+ ,e ,(? number? n2)) ,(? number? n1))
         `(* ,(? number? n1) (+ ,(? number? n2) ,e))
         `(* ,(? number? n1) (+ ,e ,(? number? n2))))
     (simplify `(+ ,(* n1 n2) ,(simplify `(* ,n1 ,e))))]
    
    [`(* ,(? number? n1) ,(? number? n2)) (* n1 n2)]
    [`(+ ,(? number? n1) ,(? number? n2)) (+ n1 n2)]
    [`(+ ,l ,r) `(+ ,(simplify l) ,(simplify r))]
    [`(* ,l ,r) `(* ,(simplify l) ,(simplify r))]
    [(? number? n) n]
    [(? symbol? s) s]))

; (+ (+ 520625 (* 270000 noun)) verb) = 19690720
(define part2-alt
  (let ([simplified (simplify part2-partial)])
    (match simplified
      [`(+ (+ ,n1 (* ,n2 noun)) verb)
       (let* ([dividend (- 19690720 n1)]
              [modulus n2]
              [noun (quotient  dividend modulus)]
              [verb (remainder dividend modulus)])
         (+ (* 100 noun) verb))])))