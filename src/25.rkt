#lang plai

(require match-string
         "../lib.rkt"
         "IntCode.rkt")

(define input
  (string->program (car (problem-input 25))))

(define (string->ascii str)
  (map char->integer (string->list str)))

(define (ascii->string lst)
  (list->string (map integer->char lst)))

(define-values (north south east west inv)
  (values (string->ascii "north\n")
          (string->ascii "south\n")
          (string->ascii "east\n")
          (string->ascii "west\n")
          (string->ascii "inv\n")))

(define (take* item)
  (let ([item-str (string-join (map symbol->string item))])
    (string->ascii (string-append "take " item-str "\n"))))

(define (drop* item)
  (let ([item-str (string-join (map symbol->string item))])
    (string->ascii (string-append "drop " item-str "\n"))))

(define (resume-with-inputs st inputs)
  (if (empty? inputs) st
      (type-case state st
        [in (resume)
            (resume-with-inputs (resume (first inputs)) (rest inputs))]
        [else (error "Unexpected program state.")])))

(define (resume-with-inputs* st inputs)
  (if (empty? inputs) st
      (type-case state st
        [in (resume)
            (if (empty? inputs) st
                (resume-with-inputs* (resume (first inputs)) (rest inputs)))]
        [out (value resume)
             (resume-with-inputs* (resume) inputs)]
        [halt (_) (error "Unexpected program state.")])))

(define (play st)
  (let loop ([st st]
             [output '()])
    (type-case state st
      [out (value resume)
           (loop (resume) (cons value output))]
      [in (resume)
          (display (ascii->string (reverse output)))
          (match (read)
            ['(north)   (loop (resume-with-inputs st north) '())]
            ['(south)   (loop (resume-with-inputs st south) '())]
            ['(east)    (loop (resume-with-inputs st east)  '())]
            ['(west)    (loop (resume-with-inputs st west)  '())]
            ['(inv)     (loop (resume-with-inputs st inv)   '())]
            [`(take ,s ...) (loop (resume-with-inputs st (take* s)) '())]
            [`(drop ,s ...) (loop (resume-with-inputs st (drop* s)) '())]
            ['(exit) (void)]
            [else (displayln "Invalid command. Try again.")
                  (loop st output)])]
      [halt (_) (display (ascii->string (reverse output)))])))

(define (goto-checkpoint)
  (let ([commands
         (append
          north
          (take* '(mouse))
          north
          (take* '(pointer))
          south
          south
          west
          (take* '(monolith))
          north
          west
          (take* '(food ration))
          south
          (take* '(space law space brochure))
          north
          east
          south
          south
          (take* '(sand))
          south
          west
          (take* '(asterisk))
          south
          (take* '(mutex))
          north
          east
          north
          north
          east
          south
          south
          west
          south
          inv)])
    (resume-with-inputs* (exec input) commands)))

(define-values (drop-mouse
                drop-pointer
                drop-monolith
                drop-ration
                drop-slsb
                drop-sand
                drop-asterisk
                drop-mutex
                take-mouse
                take-pointer
                take-monolith
                take-ration
                take-slsb
                take-sand
                take-asterisk
                take-mutex)
  (values (drop* '(mouse))
          (drop* '(pointer))
          (drop* '(monolith))
          (drop* '(food ration))
          (drop* '(space law space brochure))
          (drop* '(sand))
          (drop* '(asterisk))
          (drop* '(mutex))
          (take* '(mouse))
          (take* '(pointer))
          (take* '(monolith))
          (take* '(food ration))
          (take* '(space law space brochure))
          (take* '(sand))
          (take* '(asterisk))
          (take* '(mutex))))

(define drop-all
  (append drop-mouse
          drop-pointer
          drop-monolith
          drop-ration
          drop-slsb
          drop-sand
          drop-asterisk
          drop-mutex))

(define item-combinations
  (map append*
       (combinations
        (list take-mouse
              take-pointer
              take-monolith
              take-ration
              take-slsb
              #;take-sand ;; too heavy
              take-asterisk
              take-mutex))))

(define (get-password)
  (let* ([st (goto-checkpoint)])
    (let/cc k
      (for ([combo item-combinations])
        (let* ([commands (append drop-all combo east)]
               [st* (resume-with-inputs* st commands)])
          (let loop ([st st*]
                     [output '()])
            (type-case state st
              [out (value resume)
                   (loop (resume) (cons value output))]
              [in (_) (void)]
              [else (displayln "Items taken to pass security checkpoint:")
                    (display (ascii->string combo))
                    (display (ascii->string (reverse output))) (k)])))))))

;; Play using (play (exec input)).
;; Magically teleport to the security checkpoint using (play (goto-checkpoint)).
;; Print the final message with the password using (get-password).