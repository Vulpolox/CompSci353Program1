#lang racket

(require rackunit)
(require "CalculateScore.rkt") ; provides "calculate-score" function for getting bowling scores
(require "FileReader.rkt")     ; provides "data" which contains file contents

(define team->player-hash (make-immutable-hash))
(define player->scores-has (make-hash))

; pre  -- takes a list
; post -- returns a string indicating whether the input list refers to a team or a player
(define (team-or-player? input-list)
  (if (eq? (length input-list) 1)
  "team"
  "player"))

; pre  -- takes a list pertaining to a player
; post -- returns a list containing the player name as a single string
;         and a nested list of single strings representing said player's bowling game
(define (player-score-split input-list)
  (define player-name (string-join (take input-list 2) " "))
  (define player-score (drop input-list 2))
  (list player-name player-score))

; pre  -- takes a list of single character strings representing a bowling game
; post -- converts each string in the list to a integer if numerical or a character
;         otherwise and returns the updated list
(define (type-cast-game input-list)
  (define (str->char str)                            ; helper function for converting string to char
    (string-ref str 0))
  (define (char->int-if-num chr)                     ; helper function for converting char to int if numeric
    (if (char-numeric? chr)
        (- (char->integer chr) 48)
        chr))
  (define pure-char-list (map str->char input-list)) ; intermediary list that has strings converted to chars
  (map char->int-if-num pure-char-list))

; pre  -- takes data from scores.txt
; post -- hashes data into teams (keys) and lists of players (values) and returns map
(define (hash-team->players data [out-hash (make-immutable-hash)] [current-team "null"])
  (cond
    [(empty? data)
     out-hash]
    [(eq? (team-or-player? (first data))
          "team")
     (hash-team->players (cdr data) out-hash (first data))]
    [(eq? (team-or-player? (first data))
          "player")
     (begin
       (define new-hash (hash-set out-hash
                                  current-team
                                  (first (player-score-split (first data)))))
       (hash-team->players (cdr data) new-hash current-team))]
    )
  )
     
 (define team->players# (hash-team->players data))
team->players#
data


;(define test (second data))
;(team-or-player? test)
;(player-score-split test)
;(map team-or-player? data)