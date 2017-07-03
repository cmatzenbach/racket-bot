; http://www.chegg.com/homework-help/questions-and-answers/task-implement-simple-chatbot-racket--s-sample-chat-program-carry-chat-name-hi-name-talk-a-q20637753

; CHRIS-BOT
#lang racket

; Use the random function implemented in Racket
; (random k) returns a random integer in the range 0 to k-1
(#%require (only racket/base random))
; library for string-contains
(require srfi/13)

; I/O Helper Functions

; prompt: nil -> list
; Prompt user for input
(define (prompt)
  (newline)
  (display "talk to me >>>")
  (read-user-line))

; read-user-line: nil -> list
; Read user input until end of character
(define (read-user-line)
  (let ((next(read)))
    (if (eof-object? next)
        '()
        (cons next (read-user-line)))))

; output: list -> nil
; Display output from list of chars
(define (output lst)
  (newline)
  (display (to-string lst))
  (newline))

; to-string: list -> string
; Convert list to printable string
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

; chat-loop: string -> (func)
; Main loop control; gets input, make sure it is not exit, then calls response
(define (chat-loop name)
  (let ((input(prompt)))   ; get user input
    (if (eqv? (car input) 'bye)
        (begin
          (output(list 'bye name))
          (output(list 'have 'a 'great 'day!)))
        (begin
          (reply input name)
          (chat-loop name)))))

; reply: list string -> (func)
(define (reply input name)
  (cond
    [(string-contains (string-downcase (to-string input)) "why") (output(list 'Why 'not?))]
    [(string-contains (string-downcase (to-string input)) "how") (output(how-response))]
    [(string-contains (string-downcase (to-string input)) "what") (output(what-response))]
    [(string-contains (string-downcase (to-string input)) "because") (output(bc-response))]
    [(and (string-contains (string-downcase (to-string input)) "i") (string-contains-mult (to-string input) '("need" "think" "have" "want"))) (output(i-modal-response))]
    [(not(null? (string-contains-mult (to-string input) '("do" "can" "could" "must" "should" "will" "would")))) (output(modal-response (string-contains-mult (to-string input) '("do" "can" "could" "must" "should" "will" "would")) name))]
    [else (output (pick-random generic-response))]
    ))

; pick-random: int -> int
; Takes in limit and returns random int within said limit
(define (pick-random choices)
  (list-ref choices(random(length choices))))

(define generic-response '((that\'s nice)
                           (good to know)
                           (can you elaborate on that?)))

; modal-response: list string -> response
; Gives a yes or no response to any comments using modal verbs
(define (modal-response verb person)
  ;random gen either 1 or 2
  (define num (random 2))
  (if (= 1 num)
      (list 'Yes 'I verb person)
      ;; example: (map string->symbol '("would"))
      (list 'No 'I (to-string verb) 'not person)))

(define (how-response)
  ;random gen either 1 or 2
  (let ([x (random 2)])
    (if (= 1 x)
        (list 'Why 'do 'you 'ask?)
        (list 'How 'would 'an 'answer 'to 'that 'help 'you?))))

(define (what-response)
  ;random gen either 1 or 2
  (let ([x (random 2)])
    (if (= 1 x)
        (list 'What 'do 'you 'think?)
        (list 'Why 'do 'you 'ask?))))

(define (bc-response)
  ;random gen either 1 or 2
        (list 'Is 'that 'the 'real 'reason?))

(define (i-modal-response verb action)
  (list 'Why 'do 'you verb action))

; string-contains-mult: string list -> bool
; Takes in a list of strings and determines if target appears in list
(define (string-contains-mult str lst)
  ; lambda is a generic function def, filter applies it to each item in list and
  ; returns the result of any that are true, then string->symbol to change the ""
  ; item into a symbol for list concat, need car to take car of '("item") to get string
  (string->symbol(car (filter (lambda (elem) (string-contains (string-downcase str) elem)) lst))))


(chat-with 'Chris)
