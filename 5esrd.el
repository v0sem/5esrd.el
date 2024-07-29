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
;;; License:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
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

;;;; Structures

(cl-defstruct monster
  name
  size
  type
  subtype
  alignment
  ac
  hp
  speed
  stats
  svthrow
  skills
  inmunities
  senses
  lang
  challenge
  effects
  actions
  legenactions
  extra)

;;;; Database Functions

(defun 5esrd-load-monsters ()
  "Load monsters from monsters.el file."
  (load (buffer-file-name (find-file-noselect "monsters.el"))))

(defun 5esrd-save-monsters ()
  "Save monsters to monsters.el file."
  (save-excursion
    (let ((buf (find-file-noselect "monsters.el")))
      (set-buffer buf)
      (erase-buffer)
      (5esrd--save-list *monsters* buf)
      (print (list 'setq '*monsters* (list 'quote varlist)) buf)
      (save-buffer)
      (kill-buffer))))

(defun 5esrd--save-list (varlist buf)
  "Takes VARLIST and save it onto BUF. Auxiliar function."
  (cl-loop for monster in varlist do
           (print (list 'setq monster (list 'quote (symbol-value monster))) buf)))

(defun 5esrd-add-monster ()
  "Add a monster to the monster list."
  (add-to-list *monsters* (5esrd--save-monster (read-from-minibuffer "Name of the monster: "))))

(defun 5esrd--save-monster (mons)
  "Create a new variable MONS with the monster created by user input."
  (set (intern mons)
       (make-monster :name mons
                     :size (read-from-minibuffer "Size: ")
                     :type (read-from-minibuffer "Type: ")
                     :subtype (read-from-minibuffer "Subtype: ")
                     :alignment (read-from-minibuffer "Alignment: ")
                     :ac (read-from-minibuffer "Armor Class: ")
                     :hp (read-from-minibuffer "Hit Points: ")
                     :speed (read-from-minibuffer "Speed: ")
                     :stats (5esrd--ask-stats)
                     :svthrow (read-from-minibuffer "Saving Throws: ")
                     :skills (read-from-minibuffer "Skills: ")
                     :inmunities (read-from-minibuffer "Inmunities: ")
                     :senses (read-from-minibuffer "Senses: ")
                     :lang (read-from-minibuffer "Languages: ")
                     :challenge (read-from-minibuffer "Challenge: ")
                     :effects (5esrd--ask-name-list "effect")
                     :actions (5esrd--ask-name-list "action")
                     :legenactions (5esrd--ask-name-list "legendary action")
                     :extra (5esrd--ask-name-list "extra info"))))

(defun 5esrd--ask-stats ()
  "Return a list with stats with the minibuffer input."
  (cl-loop for stat in '("Strength: " "Dexterity: " "Constitution: " "Intelligence: " "Wisdom: " "Charisma: ") collect
           (string-to-number (read-from-minibuffer stat))))

(defun 5esrd--ask-name-list (category)
  "Ask the user to input a series of data for CATEGORY."
  (cl-loop while (y-or-n-p (concat "Add new " category "? "))
           collect (list (read-from-minibuffer "Name: ") (read-from-minibuffer "Description: "))))

;;;;; Public

(defun 5esrd-get-monster ()
  (let ((monster-search
         (cl-loop for mon in *monsters* collect
                  (list (monster-name (symbol-value mon)) (symbol-value mon)))))
    (cadr (assoc (completing-read
                  "Search for a monster: "
                  monster-search
                  ) monster-search))))

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
