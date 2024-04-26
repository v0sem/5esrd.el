;;; 5esrd.el --- Tools for writing D&D content -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Vosem
;;
;; Author: Vosem <progpabsanch@gmail.com>
;; Maintainer: Vosem <progpabsanch@gmail.com>
;; Created: abril 24, 2024
;; Modified: abril 24, 2024
;; Version: 0.0.1
;; Keywords: games abbrev
;; Homepage: https://github.com/vosem/5esrd
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; This package provides various functions for automatically fetching SRD
;; content or other useful D&D related values.
;;
;;; Code:
;;
;;;; Requirements:

(require 'cl-lib)
(require 'subr-x)

;;;;Functions

;;;;; Public

(defun 5esrd-roll ()
  "Read from minibuffer and roll."
  (let ((roll (5esrd--str-roll (read-from-minibuffer "Roll:"))))
    (number-to-string (+ (seq-reduce #'+ (car roll) 0) (car (cdr roll))))))

;;;;; Private
(defun 5esrd--roll-die (die)
  "Return a number from 1 to DIE."
  (+ 1 (random die)))

(defun 5esrd--roll-dice (amnt dice)
  "Return a list of AMNT rolls with DICE."
  (let ((rolls nil))
    (dotimes (i amnt)
      (setf rolls (cons (5esrd--roll-die dice) rolls)))
    rolls))

(defun 5esrd--str-roll (str)
  "Read roll from STR and extract values."
  (let ((parts (split-string str "d")))
    (when (not (length= parts 2))
      (error "Wrong format"))
    (let ((modpos (split-string (car (cdr parts)) "+")))
      (if (length= modpos 2)
          (list (5esrd--roll-dice
                 (cl-parse-integer (car parts))
                 (cl-parse-integer (car modpos)))
                (cl-parse-integer (car (cdr modpos))))
        (let ((modneg (split-string (car (cdr parts)) "-")))
          (if (length= modneg 2)
              (list (5esrd--roll-dice
                     (cl-parse-integer (car parts))
                     (cl-parse-integer (car modneg)))
                    (* -1 (cl-parse-integer (car (cdr modneg)))))
            (list (5esrd--roll-dice
                   (cl-parse-integer (car parts))
                   (cl-parse-integer (car modneg)))
                  0)))))))

(provide '5esrd)
;;; 5esrd.el ends here
