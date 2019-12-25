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
            (if (empty? inputs) st
                (resume-with-inputs (resume (first inputs)) (rest inputs)))]
        [out (value resume)
             #;(display (integer->char value))
             (resume-with-inputs (resume) inputs)]
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

(define drop-mouse    (drop* '(mouse)))
(define drop-pointer  (drop* '(pointer)))
(define drop-monolith (drop* '(monolith)))
(define drop-ration   (drop* '(food ration)))
(define drop-slsb     (drop* '(space law space brochure)))
(define drop-sand     (drop* '(sand)))
(define drop-asterisk (drop* '(asterisk)))
(define drop-mutex    (drop* '(mutex)))
(define take-mouse    (take* '(mouse)))
(define take-pointer  (take* '(pointer)))
(define take-monolith (take* '(monolith)))
(define take-ration   (take* '(food ration)))
(define take-slsb     (take* '(space law space brochure)))
(define take-sand     (take* '(sand)))
(define take-asterisk (take* '(asterisk)))
(define take-mutex    (take* '(mutex)))

;; Some other locations we don't need to explore:
;; - quarters: east of storage
;; - holodeck: east of hallway
;; - gift wrapping: east of kitchen
;; - passage: south of gift wrapping
;; Some items we should not take:
;; - lava: in sick bay
;; - electromagnet: in gift wrapping
;; - escape pod: in holodeck
;; - photons: in engineering
(define (goto-checkpoint)
  (let ([commands
         (append
          north ;; hallway
          take-mouse
          north ;; kitchen
          take-pointer
          south south west ;; hallway, hull, stable
          take-monolith
          north west ;; sick bay, observatory
          take-ration
          south ;; hot chocolate fountain
          take-slsb
          north east south south ;; observatory, sick bay, stable, navigation
          take-sand
          south west ;; corridor, maintenance
          take-asterisk
          south ;; storage
          take-mutex
          ;; maintenance, corridor, navigation, stable, hull,
          ;; arcade, laboratory, engineering, checkpoint
          north east north north east south south west south
          inv)])
    (resume-with-inputs (exec input) commands)))

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
               [st* (resume-with-inputs st commands)])
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