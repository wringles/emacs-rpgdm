;;; rpgdm-tables-dice.el --- Rolling dice for choosing items from Tables -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Howard X. Abrams
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams <howard.abrams@gmail.com>
;; Created: February  5, 2021
;;
;; This file is not part of GNU Emacs.
;;
;;
;;; Commentary:
;;
;;     This file contains the source code, but the concept behind what I'm
;;     calling random dice tables is a bit complex, so I recommend looking
;;     at the original file in `docs/rpgdm-tables-dice.org'.

;;; Code:

(defstruct dice-table dice rows)

(defun rpgdm-tables--choose-dice-table (table)
  "Choose a string from a random dice table."
  (let* ((roll (rpgdm-roll-sum (dice-table-dice table)))
         (rows (dice-table-rows table))
         (results (rpgdm-tables-dice--choose roll rows)))
    (if (stringp results)
        results
        (seq-random-elt results))))

(defun rpgdm-tables-dice--choose (roll rows)
  "Given a numeric ROLL, return row that matches.
This assumes ROWS is a sorted list where the first element (the
`car') is a numeric level that if ROLL is less than or equal, we
return the `rest' of the row. Otherwise, we recursively call this
function with the `rest' of the rows."
  (let* ((row (first rows))
         (level (car row))
         (answer (rest row)))

    (if (<= roll level)
        answer
      (rpgdm-tables-dice--choose roll (rest rows)))))

(setq rpgdm-tables-dice-table-regexp (rx "Roll"
                                           (one-or-more space)
                                           (optional (or "on" "for"))
                                           (zero-or-more space)
                                           "Table:"
                                           (zero-or-more space)
                                           (group
                                            (regexp rpgdm-roll-regexp))))

(defun rpgdm-tables-dice-table? ()
  "Return non-nil if current buffer contains a dice-table"
  (goto-char (point-min))
  (re-search-forward rpgdm-tables-dice-table-regexp nil t))

(defun rpgdm-tables--parse-as-dice-table ()
  "Return `dice-table' of lines matching `rpgdm-tables-dice-table-rows'."
  (let ((dice (match-string-no-properties 1))         ; Grab expression before moving on
        (rows ())                                     ; Modify this with add-to-list
        (row-splitter (rx (* space) "|" (* space))))  ; Split rest of table row

    (while (re-search-forward rgpdm-tables-dice-table-rows nil t)
      (let* ((levelstr (match-string-no-properties 1))
             (level    (string-to-number levelstr))
             (row      (match-string-no-properties 2))
             (choices  (split-string row row-splitter t)))
        (add-to-list 'rows (cons level choices))))
    (make-dice-table :dice dice
                     :rows (sort rows (lambda (a b) (< (first a) (first b)))))))

(setq rgpdm-tables-dice-table-rows (rx bol
                                         (zero-or-more space) "|" (zero-or-more space)
                                         (optional (one-or-more digit)
                                                   (one-or-more "-"))
                                         (group
                                          (one-or-more digit))
                                         (zero-or-more space) "|" (zero-or-more space)
                                         (group (+? any))
                                         (zero-or-more space) "|" (zero-or-more space)
                                         eol))

(provide 'rpgdm-tables-dice)
;;; rpgdm-tables-dice.el ends here
