;;; rpgdm-tables.el --- Choose table from Tables -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Howard X. Abrams
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams <howard.abrams@workday.com>
;; Created: January  8, 2021
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; By storing tables in files, we can preparse them, allowing us to randomly
;; choose entries. The primary interface are:
;;
;;   - `rpgdm-tables-load' :: Which when pointed to a directory, stores the data
;;                            in all text files read.
;;
;;   - `rpgdm-tables-choose' :: Which, when a table is choosen, returns a random
;;                              element.
;;
;; The files read can be simply formatted as lists, or as an org-mode table. If
;; a table is found, the second column should be a _frequency_ label, where some
;; items in that table will be chosen more often than others.
;;
;;; Code:

(defvar rpgdm-base ".")
(require 'rpgdm-dice (expand-file-name "rpgdm-dice.el" rpgdm-base) t)
(require 'rpgdm-tables-freq (expand-file-name "rpgdm-tables-freq.el" rpgdm-base) t)
(require 'rpgdm-tables-dice (expand-file-name "rpgdm-tables-dice.el" rpgdm-base) t)


(defvar rpgdm-tables-directory
  (expand-file-name "tables" rpgdm-base)
  "Directory path containing the tables to load and create functions.")

(defvar rpgdm-tables (make-hash-table :test 'equal)
  "Collection of tables and lists for the Dungeon Master.")


(defun rpgdm-tables-load (&optional filepath)
  "Read and parse table files located in FILEPATH directory.
Files will be stored in `rpgdm-tables' hash, and available by name
when calling `rpgdm-tables-choose'."
  (interactive (list (read-directory-name "DM Tables Directory: " rpgdm-tables-directory)))
  (rpgdm-tables--load-dir filepath)
  (message "Read: %s" (s-join ", " (hash-table-keys rpgdm-tables))))

(defun rpgdm-tables--load-dir (filepath &optional prefix)
  "Read and parse the files in the directory given by FILEPATH.
PREFIX all tables if this parameter is given."
  (dolist (file (directory-files filepath t))
    (let ((new-prefix (s-join "/" (-flatten (list prefix (file-name-base file))))))
      (message "Reading: %s (%s)" file new-prefix)
      (cond ((file-directory-p file)    (rpgdm-tables--load-dir file new-prefix))
            ((and (file-regular-p file) (file-readable-p file))
             (rpgdm-tables--load-file file prefix))))))

(defun rpgdm-tables--load-file (filepath prefix)
  "Read, parse and store the table given by FILEPATH.
PREFIX the table name, if given."
  (let ((name (file-name-base filepath))
        (contents (rpgdm-tables--read-table-file filepath)))
    (if prefix
        (setq name (format "%s/%s" prefix name)))
    ;; (message "Read: %s" table-file)
    (puthash name contents rpgdm-tables)))


(defun rpgdm-tables-choose (table-name)
  "Return random item from a table of a given TABLE-NAME string.

The name is searched in the `rpgdm-tables' hash-table, and the
value returned is a _table_ of sorts. It could be a list, which
would be easy (see `rpgdm-tables--choose-list'), or it is either
a freq-uency table (see `rpgdm-tables--choose-freq-table') or a
dice table (see `rpgdm-tables--choose-dice-table')."
  (interactive (list (completing-read "Choose from Table: " (sort (hash-table-keys rpgdm-tables) #'string-lessp))))

  (let* ((table    (gethash table-name rpgdm-tables))
         (result   (cond ((dice-table-p table) (rpgdm-tables--choose-dice-table table))
                         ((hash-table-p table) (rpgdm-tables--choose-freq-table table))
                         ((listp table)        (rpgdm-tables--choose-list table))
                         (t "Error: Could choose anything from %s (internal bug?)" table-name)))
         ;; Replace any dice expression in the message with an roll:
         (dice-sum (lambda (dice-exp) (number-to-string (rpgdm-roll-sum dice-exp))))
         (no-dice-nums  (replace-regexp-in-string rpgdm-roll-regexp dice-sum result))
         (no-alt-words  (rpgdm-tables--choose-string-list no-dice-nums)))
    (rpgdm-message "%s" no-alt-words)))


(defun rpgdm-tables--choose-list (lst)
  "Randomly choose (equal chance for any) element in LST."
  (seq-random-elt lst))

(defun rpgdm-tables--choose-string-list (str)
  "Return a substituted item from _string-list_ in STR.
For instance, the string: 'You found a [chair/box/statue]'
would be converted randomly to something like: 'You found a box.'"
  (let ((regexp (rx "[" (+? any) "/" (+? any) "]"))
        (subbed (lambda (str) (--> str
                           (substring it 1 -1)
                           (s-split (rx (*? space) "/" (*? space)) it)
                           (seq-random-elt it)))))
    (replace-regexp-in-string regexp subbed str)))


;; I originally thought that I could have a single regular expression that
;; matched all possible tables, but that is a bit too complicated. The following
;; regular expression, however, will parse a list or a frequency table.

(defvar rpgdm-tables--line-parse
  (rx bol (zero-or-more space)
      (optional
       (any "+" "-" "*" "|")
       (one-or-more space))
      (group (+? any))
      (optional (zero-or-more space) (or ":" "|") (zero-or-more space)
                (group (one-or-more alphanumeric))
                (zero-or-more space) (optional (or ":" "|")) (zero-or-more space))
      eol)
  "A regular expression for locating parsable lines.")

(ert-deftest rpgdm-tables--line-parse-test ()
  "Why yes, we should test our regular expression."
  (dolist (data '((" - normal list" "normal list" nil)
                  (" + normal list" "normal list" nil)
                  (" * normal list" "normal list" nil)
                  ("normal list" "normal list" nil)
                  (" - list with tag :tag:" "list with tag" "tag")
                  (" - list with tag :tag" "list with tag" "tag")
                  (" - list with tag : tag" "list with tag" "tag")
                  (" | table cell | freq |" "table cell" "freq")))
    (cl-destructuring-bind (line contents tag) data
      (should (string-match rpgdm-tables--line-parse line))
      (should (equal (match-string 1 line) contents))
      (when tag
        (should (equal (match-string 2 line) tag))))))

(defun rpgdm-tables--read-table-file (table-file)
  "Read and parse TABLE-FILE as data. Whatever that means."
  (when (and (file-regular-p table-file) (file-readable-p table-file))
    (with-temp-buffer
      (insert-file-contents table-file)
      (goto-char (point-min))
      (flush-lines (rx bol (zero-or-more space) "#"))

      ;; The following predicates are not /pure functions/, as they scan the
      ;; current buffer, leaving the initial match in the 'hopper', so the parsing
      ;; function called makes that assumption, and will immediately grab that
      ;; `match-string' and then parse the rest of the buffer.
      (cond
       ((rpgdm-tables-dice-table?) (rpgdm-tables--parse-as-dice-table))
       ((rpgdm-tables-freq-table?) (rpgdm-tables--parse-as-freq-table))
       (t (rpgdm-tables--parse-as-list))))))

(defun rpgdm-tables--parse-as-list ()
  "Return list of lines matching `rpgdm-tables--line-parse'."
  (let ((results (list (match-string-no-properties 1))))
    (while (re-search-forward rpgdm-tables--line-parse nil t)
      (let ((entry (match-string-no-properties 1)))
        (setq results (cons entry results))))
    results))


(provide 'rpgdm-tables)
