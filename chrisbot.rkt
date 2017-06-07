; http://www.chegg.com/homework-help/questions-and-answers/task-implement-simple-chatbot-racket--s-sample-chat-program-carry-chat-name-hi-name-talk-a-q20637753

; CHRIS-BOT
#lang racket

; module containing to-string
;; (require web-server/formlets/input)
; Use the random function implemented in Racket
; (random k) returns a random integer in the range 0 to k-1
(#%require (only racket/base random))

; I/O Helper Functions

; Nil -> List
; Prompt user for input
(define (prompt)
  (newline)
  (display "talk to me >>>")
  (read-user-line))

; Nil -> List
; Read user input until end of character
(define (read-user-line)
  (let ((next(read)))
    (if (eof-object? next)
        '()
        (cons next (read-user-line)))))

; List -> Nil
; Display output from list of chars
(define (output lst)
  (newline)
  (display (to-string lst))
  (newline))

(define (to-string lst)
  (cond
    [(null? lst) ""]
    [(eq? (length lst) 1) (symbol->string(car lst))]
    [else (string-append (symbol->string(car lst)) " " (to-string (cdr lst)))]
    ))

; Main Function
; usage: (chat-with 'your-name)
(define (chat-with name)
  (output (list 'Hi name))
  (chat-loop name))

; chat loop
(define (chat-loop name)
  (let ((input(prompt)))   ; get user input
    (if (eqv? (car input) 'bye)
        (begin
          (output(list 'bye name))
          (output(list 'have 'a 'great 'day!)))
        (begin
          (reply input name)
          (chat-loop name)))))

(define (reply input name)
  (cond
    [(output (pick-random generic-response))]
    []))

(define (pick-random choices)
  (list-ref choices(random(length choices))))

(define generic-response '((that\'s nice)
                           (good to know)
                           (can you elaborate on that?)))

(chat-with 'Chris)
