#lang racket
; http://www.chegg.com/homework-help/questions-and-answers/task-implement-simple-chatbot-racket--s-sample-chat-program-carry-chat-name-hi-name-talk-a-q20637753


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
  (let (next(read))
    (if (eof-object? next)
        '()
        (cons next (read-user-line)))))

; List -> Nil
; Display output from list of chars
(define (output lst)
  )
