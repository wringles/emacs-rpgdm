;;; rpgdm-tables.el --- Choose table from Tables -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Howard X. Abrams
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams
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

(require 'ert)

(require 'rpgdm-dice)
(require 'rpgdm-tables-freq)
(require 'rpgdm-tables-dice)


(defvar rpgdm-tables-directory
  (expand-file-name "tables" rpgdm-base)
  "Directory path containing the tables to load and create functions.")

(defvar rpgdm-tables (make-hash-table :test 'equal)
  "Collection of tables and lists for the Dungeon Master.")

(defun rpgdm-tables-clear ()
  "Clear previously loaded tables."
  (interactive)
  (clrhash rpgdm-tables))

(defun rpgdm-tables-empty-p ()
  "Return non-nil if no tables have been loaded."
  (= (hash-table-count rpgdm-tables) 0))

(defun rpgdm-tables-load (&optional filepath)
  "Read and parse table files located in FILEPATH directory.
Files will be stored in `rpgdm-tables' hash, and available by name
when calling `rpgdm-tables-choose'."
  (interactive (list (read-directory-name "DM Tables Directory: " rpgdm-tables-directory)))
  (unless filepath
    (setq filepath rpgdm-tables-directory))
  (rpgdm-tables--load-dir filepath)
  (message "Read: %s" (s-join ", " (hash-table-keys rpgdm-tables))))

(defun rpgdm-tables--load-dir (filepath &optional prefix)
  "Read and parse the files in the directory given by FILEPATH.
PREFIX all tables if this parameter is given."
  (dolist (file (directory-files filepath t (rx bol (not "."))))
    (let ((new-prefix (s-join "/" (-flatten (list prefix (file-name-base file))))))
      (cond ((file-directory-p file)    (rpgdm-tables--load-dir file new-prefix))
            ((and (file-regular-p file) (file-readable-p file))
             (rpgdm-tables--almost-load-file file prefix))))))

(defun rpgdm-tables--almost-load-file (filepath &optional prefix)
  "Loading hundreds of tables takes a long time, and fills up memory.
Instead, we store a reference to FILEPATH with the optional
PREFIX, and then if we query for that table (and it is just a
string), we load it then."
  (let ((name (file-name-base filepath)))
    (if prefix
        (setq name (format "%s/%s" prefix name)))
    (puthash name filepath rpgdm-tables)))

(defun rpgdm-tables-load-file (filepath &optional name)
  "Read, parse and store the table given by FILEPATH.
Store it by NAME in the `rpgdm-tables' hash table."
  (interactive (list (read-file-name "DM Table: " rpgdm-tables-directory)))
  (when (null name)
    (setq name (file-name-base filepath)))
  (let ((contents (rpgdm-tables--read-table-file filepath)))
    (when (called-interactively-p)
      (message "Read: %s" name))
    (puthash name contents rpgdm-tables)))


(defun rpgdm-tables-choose (table-name)
  "Return random item from a table of a given TABLE-NAME string.

The name is searched in the `rpgdm-tables' hash-table, and the
value returned is a _table_ of sorts. It could be a list, which
would be easy (see `rpgdm-tables--choose-list'), or it is either
a freq-uency table (see `rpgdm-tables--choose-freq-table') or a
dice table (see `rpgdm-tables--choose-dice-table')."
  (interactive (list (completing-read "Choose from Table: "
                                      (sort (hash-table-keys rpgdm-tables) #'string-lessp))))
  (when-let ((table  (gethash table-name rpgdm-tables)))
    (when (stringp table)
      (setq table (rpgdm-tables-load-file table table-name)))
    (let* ((result   (cond ((dice-table-p table) (rpgdm-tables--choose-dice-table table))
                           ((hash-table-p table) (rpgdm-tables--choose-freq-table table))
			   ((functionp table)    (call-interactively table))
                           ((listp table)        (rpgdm-tables--choose-list table))
                           (t "Error: Could choose anything from %s (internal bug?)" table-name)))
           ;; Replace any dice expression in the message with an roll:
           (dice-sum (lambda (dice-exp) (number-to-string (rpgdm-roll-sum dice-exp))))
           (no-dice-nums  (replace-regexp-in-string rpgdm-roll-regexp dice-sum result))
           (no-alt-words  (rpgdm-tables--choose-string-list no-dice-nums)))
      (kill-new no-alt-words)
      (rpgdm-message "%s" no-alt-words))))

(defun rpgdm-tables--choose-list (lst)
  "Randomly choose (equal chance for any) element in LST."
  (when lst
    (seq-random-elt lst)))

(defun rpgdm-tables--choose-string-list (str)
  "Return a substituted item from _string-list_ in STR.
For instance, the string: 'You found a [chair/box/statue]'
would be converted randomly to something like: 'You found a box.'"
  (let ((regexp (rx "[" (+? any) "/" (+? any) "]"))
	(subbed (lambda (str) (--> str
				   (substring it 1 -1)
				   (s-split (rx "/") it)
				   (seq-random-elt it)
				   (s-trim it)))))
    (replace-regexp-in-string regexp subbed str)))

(ert-deftest rpgdm-tables--choose-string-list ()
  (let ((empty-string "")
	(no-op-string "This is just a phrase.")
	(two-choices  "We can have [this/that]")
	(two-choices1 "We can have this")
	(two-choices2 "We can have that")
	(tri-choices  "We can have [this / that / the other].")
	(tri-choices1 "We can have this.")
	(tri-choices2 "We can have that.")
	(tri-choices3 "We can have the other."))

    (should (equal (rpgdm-tables--choose-string-list empty-string) empty-string))
    (should (equal (rpgdm-tables--choose-string-list no-op-string) no-op-string))
    (let ((chosen (rpgdm-tables--choose-string-list two-choices)))
      (should (or (equal chosen two-choices1) (equal chosen two-choices2))))
    (let ((chosen (rpgdm-tables--choose-string-list tri-choices)))
      (should (or (equal chosen tri-choices1) (equal chosen tri-choices2) (equal chosen tri-choices3))))))

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

      ;; I noticed that org-mode links we screwing up the output, so we strip them out:

      (goto-char (point-min))
      (let ((org-link-re (rx "[[" (one-or-more (not "]")) "]["
                             (group (one-or-more (not "]")))
                             "]]")))
        (while (re-search-forward org-link-re nil t)
          (replace-match (match-string 1) nil nil)))

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
