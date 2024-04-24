;;; 5esrd.el --- Tools for writing D&D content from emacs
;;
;; Copyright (C) 2024 Vosem
;;
;; Author: Vosem <vosem@gr0g>
;; Maintainer: Vosem <vosem@gr0g>
;; Created: abril 24, 2024
;; Modified: abril 24, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/vosem/5esrd
;; Package-Requires: ((emacs "24.3"))
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

;;;;Functions

;;;;; Public



;;;;; Private
(defun 5esrd--roll-die (die)
  "Return a number from 1 to DIE."
  (+ 1 (random die)))

(defun 5esrd--roll-dice (amnt dice)
  "Return a list of AMNT rolls with DICE."
  (let ((rolls nil))
    (dotimes (roll amnt)
      (setf rolls (cons (5esrd-roll-die dice) rolls)))
    rolls))

(provide '5esrd)
;;; 5esrd.el ends here
