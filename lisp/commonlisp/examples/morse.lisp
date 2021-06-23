; learn some vim slime
(defpackage :morse
  (:use :common-lisp))

(in-package :morse)

; editing

(declaim (optimize (speed 0) (safety 3) (debug 3)))

; ,W -> wrap ()
; ,< -> move ( or ) to the left
; ,> -> move ( or ) to the right
; ,S -> remove the wrapping ()

; loading
; ,F -> compile the fiel
; ,L -> reload

; info
; ,s -> describe symbol
; ,A -> prorpos

; repl
; ,c -> connect to swan server
; ,y -> interrupt lisp process
; ,- -> clear
; ,Q -> quit repl
; ,g -> set package

; cross reference
; ,xc -> who calls

; evaluation
; ,d -> deval defun (topve level form)
; ,e -> eval current expression
; ,r -> eval visual selected region
; ,b -> eval the buffer
; ,v -> interactive eval
; ,u -> undefine function

; profiling
; ,p toggle profile
; ,o profile report
; ,x profile reset

(defparameter *morse-mapping*
  '((#\A ".-")
    (#\B "-...")
    (#\C "-.-.")
    (#\D "-..")
    (#\E ".")
    (#\F "..-.")
    (#\G "--.")
    (#\H "....")
    (#\I "..")
    (#\J ".---")
    (#\K "-.-")
    (#\L ".-..")
    (#\M "--")
    (#\N "-.")
    (#\O "---")
    (#\P ".--.")
    (#\Q "--.-")
    (#\R ".-.")
    (#\S "...")
    (#\T "-")
    (#\U "..-")
    (#\V "...-")
    (#\W ".--")
    (#\X "-..-")
    (#\Y "-.--")
    (#\Z "--..")
    ))


(defun character-to-morse (character)
  (second (assoc character *morse-mapping* :test #'char-equal)))

(defun morse-to-character (morse-string)
  (first
    (find morse-string *morse-mapping* :test #'string= :key #'second)))

(defun string-to-morse (string)
  (with-output-to-string (morse)
    (write-string (character-to-morse (aref string 0)) morse)
    (loop
      for char across (subseq string 1)
      do (write-char #\Space morse)
      do (write-string) (character-to-morse char) morse)))

(defun morse-to-string (string)
  (with-output-to-string (character-stream)
    (loop
      for morse-char
        in (split-sequence:split-sequence #\Space string)
      do (write-char (morse-to-character morse-char) character-stream))))


(character-to-morse #\A)
(character-to-morse #\A)
(morse-to-character "-")
(morse-to-character ".--")
(morse-to-character ".--")
(morse-to-character "..")

(string-to-morse "WHYLISPUSESOMANYUPPERCASES")
(string-to-morse "AVOIDUSINGCONTROLC")
(string-to-morse "HAHA")
