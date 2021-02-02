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

(defvar rpgdm-tables-directory
  (expand-file-name "tables" rpgdm-base)
  "Directory path containing the tables to load and create functions.")

(defvar rpgdm-tables (make-hash-table :test 'equal)
  "Collection of tables and lists for the Dungeon Master.")


(defun rpgdm-tables-load (&optional filepath)
  "Create functions from table files located in FILEPATH directory."
  (interactive (list (read-directory-name "DM Tables Directory: " rpgdm-tables-directory)))
  (dolist (table-file (directory-files filepath t))
    (let ((name (file-name-base table-file))
          (contents (rpgdm-tables--read-table-file table-file)))
      ;; (message "Read: %s" table-file)
      (puthash name contents rpgdm-tables)))
  (message "Read: %s" (s-join ", " (hash-table-keys rpgdm-tables))))

(defun rpgdm-tables-choose (table-name)
  "Given a TABLE-NAME string, pick a random item from that table."
  (interactive (list (completing-read "Choose from Table: " (hash-table-keys rpgdm-tables))))
  (let ((table (gethash table-name rpgdm-tables)))
    (cond ((listp table)        (rpgdm-tables--choose-list table))
          ((hash-table-p table) (rpgdm-tables--choose-hash table))
          (t "Error: Could choose anything from %s (internal bug?)" table-name))))


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
  (with-temp-buffer
    (insert-file-contents table-file)
    (goto-char (point-min))
    (flush-lines (rx bol (zero-or-more space) "#"))
    (when (re-search-forward rpgdm-tables--line-parse nil nil)
      (if (match-string 2)
          (rpgdm-tables--parse-as-hash)
        (rpgdm-tables--parse-as-list)))))

(defun rpgdm-tables--parse-as-list ()
  "Return list of lines matching `rpgdm-tables--line-parse'."
  (let ((results (list (match-string-no-properties 1))))
    (while (re-search-forward rpgdm-tables--line-parse nil t)
      (let ((entry (match-string-no-properties 1)))
        (setq results (cons entry results))))
    results))

(defun rpgdm-tables--parse-as-hash ()
  "Return hashtable of lines matching `rpgdm-tables--line-parse'.
The keys in the hashtable are the tags from the file, and the
values will be a list of matching entries of that tag.

For instance, the file:
  - salt :common:
  - silver :rare:
  - gold :rare:
  - pepper :common:

Would return a hashtable containing:

  rare: [gold silver]
  common [salt peper]"
  ;; Create a hash, populated it, and return it:
  (let ((results (make-hash-table :test 'equal))
        (entry (match-string-no-properties 1))
        (tag (match-string-no-properties 2)))
    ;; Store initial match from parent call:
    (puthash tag (list entry) results)

    (while (re-search-forward rpgdm-tables--line-parse nil t)
      (let* ((entry (match-string-no-properties 1))
             (tag (match-string-no-properties 2))
             (prev-list (gethash tag results)))
        (puthash tag (cons entry prev-list) results)))

    ;; Combine the sublists of equivalent tags:
    (rpgdm-tables--flatten-hash results)))

(defun rpgdm-tables--flatten-hash (table)
  "Combine the values of equivalent table-tags in TABLE.
A table, read as a hash table, may have similar, but equal tags.
For instance, `veryrare' and `very-rare' are the same."
  (let* ((table-tags (rpgdm-tables--which-tag-group table))  ; Ignore all the numbers
         (tags (-map 'rest table-tags)))
    (dolist (subtag-list tags)
      (unless (= 1 (length subtag-list))
        (let ((keeper (first subtag-list)))
          (dolist (tag (rest subtag-list))
            (puthash keeper (append (gethash keeper table)
                                    (gethash tag table)) table)
            (remhash tag table)))))
    table))

;; ----------------------------------------------------------------------
;;
;; At this point, we have a hash table of tables, where the key is the name, and
;; the value is either:
;;   - A list of things to choose
;;   - A hash table where the key is a tag, and the value is a list of items
;;     that match that tag.
;;
;; Obviously, selecting an element from a list is easy:

(defun rpgdm-tables--choose-list (lst)
  "Randomly choose (equal chance for any) element in LST."
  (let ((item (seq-random-elt lst)))
    (rpgdm-message "%s" item)))

;; However, choosing an element in a hash of tags seems ... challenging. This is
;; because I want the tags to somehow add a particular weight to the randomness.
;; Not a complete standard distribution (bell curve), but a little more favor to
;; some items. For instance, labeling something 'common' should show more often
;; than 'uncommon'.
;;
;; Choosing an item from a hash table is a complicated algorithm that may not be
;; as obvious by reading the code, so let's describe this with an example.
;; Assume we have the following frequency table with a /relative weight/ for each tag:
;;
;;   - often :: 4
;;   - seldom :: 3
;;   - scarely :: 2
;;   - rarely :: 1
;;
;; Is coded with the following list of lists:
;;
;;    ((4 "often")
;;     (3 "seldom" "sometimes")
;;     (2 "scarcely" "scarce" "hardly ever")
;;     (1 "rarely"))
;;
;; Read this as: we should have 4 times as many items labeled "often" as "rarely".
;;
;; So we use the function, `rpgdm-tables--table-distribution' to make a
;; table-specific tag list, usually called `table-tags', where:
;;
;;    each weight = the number of items * relative weight
;;
;; So if we had 11 items in the table tagged as "often", and 8 rare
;; items, we would have a tag table as:
;;
;;    ((44 "often") (27 "seldom") (22 "scarcely") (8 "rarely"))

;; TODO English tags and their relative weights are hard-coded at the moment.
;; This really should just be part of the file, perhaps as a buffer-local
;; variable?

(defconst rpgdm-tables-tag-groups
  '(((10 "common")
     (6 "uncommon")
     (4 "rare")
     (2 "veryrare" "very-rare" "very rare")
     (1 "legendary"))

    ((4 "often")
     (3 "seldom" "sometimes")
     (2 "scarcely" "scarce" "hardly ever")
     (1 "rarely"))))

(defun rpgdm-tables--choose-hash (table)
  "Select item from a hash TABLE.
Note that tables stored in a hash table have weight keys and a list
of items associated with that weight."
  (let* ((table-tags (rpgdm-tables--table-distribution table))
         (tag (rpgdm-tables--choose-tag table-tags)))
    (rpgdm-tables--choose-list (gethash tag table))))


(defun rpgdm-tables--relevel-table (table tag)
  "Given a TAG of a hash TABLE, return new relative level.
The new value is based on the original weight, e.g. 4 and the
number of items of that particular tag.

Note: This is a helper function for `rpgdm-tables--table-distribution'."
  (let* ((name  (second tag))
         (items (gethash name table))
         (weight (first tag))
         (new-weight (* weight (length items))))
    (list new-weight name)))

(ert-deftest rpgdm-tables-relevel-table-test ()
  ;; Need to make a fake table, so we will just have a single entry in this
  ;; table, with a tag of "often". We'll specify that the weight for this should
  ;; be 4, and we'll store 10 items under that tag:
  (let* ((table (make-hash-table :test 'equal))
         (tag "often")
         (tag-weight-tuple (list 4 tag)))
    (puthash tag (number-sequence 1 10) table)
    (should (equal (list 40 tag)
                   (rpgdm-tables--relevel-table table tag-weight-tuple)))))


(defun rpgdm-tables--table-distribution (table)
  "Return a relative frequency tag group for a given TABLE.
Works by running map over the table's tags through the
`rpgdm-tables--relevel-table' helper function."
  (let ((table-tags (rpgdm-tables--which-tag-group table)))
    (--map (rpgdm-tables--relevel-table table it) table-tags)))


(defun rpgdm-tables--sum-tag-weights (tags)
  "The TAGS is a list of lists where the first element is a numeric weight.
Using `-reduce' allows us to sum these, but we need to make sure that the
first element of our list is our initial condition, so we `cons' a 0 onto
the start."
  (--reduce (+ acc (first it)) (cons 0 tags)))

(ert-deftest rpgdm-tables--sum--tag-weights-test ()
  (let ((weighted-tags
         '((44 "often") (27 "seldom") (22 "scarcely") (7 "rarely"))))
    (should (= 100 (rpgdm-tables--sum-tag-weights weighted-tags)))))


(defun rpgdm-tables--find-tag (roll tag-list)
  "Given a ROLL as a level in TAG-LIST, return matching tag.
The matching is based on the weight. A million ways to do this,
but stepping through the list of tags to see roll is in that
*window*, and if not, both move to the next tag, as well as
decrement the ROLL value."
  (cl-loop for (num-elems tag) in tag-list do
           ;; (message "Comparing %d <= %d for %s" roll num-elems tag)
           (if (<= roll num-elems)
               (return tag)
             (decf roll num-elems))))

(ert-deftest rpgdm-tables--find-tag-test ()
  (let ((weighted-tags
         '((44 "often") (27 "seldom") (22 "scarcely") (7 "rarely"))))
    (should (equal "often"    (rpgdm-tables--find-tag 1 weighted-tags)))
    (should (equal "often"    (rpgdm-tables--find-tag 44 weighted-tags)))
    (should (equal "seldom"   (rpgdm-tables--find-tag 45 weighted-tags)))
    (should (equal "seldom"   (rpgdm-tables--find-tag 71 weighted-tags)))
    (should (equal "scarcely" (rpgdm-tables--find-tag 72 weighted-tags)))
    (should (equal "scarcely" (rpgdm-tables--find-tag 93 weighted-tags)))
    (should (equal "rarely"   (rpgdm-tables--find-tag 94 weighted-tags)))
    (should (equal "rarely"   (rpgdm-tables--find-tag 100 weighted-tags)))))


(defun rpgdm-tables--choose-tag (tags)
  "Select random tag from TAGS in `rpgdm-tables-tag-groups'.
Uses helper function, `rpgdm-tables--find-tag'."
  (let* ((upper-limit (rpgdm-tables--sum-tag-weights tags))
         (roll (rpgdm--roll-die upper-limit)))
    ;; (message "Rolled %d on %d" roll upper-limit)
    (rpgdm-tables--find-tag roll tags)))

(defun rpgdm-tables--which-tag-group (table)
  "Return the tag table-tags associated with TABLE."
  (let (results
        (tag (first (hash-table-keys table))))
    (dolist (table-tags rpgdm-tables-tag-groups results)
      (let ((tag-list (->> table-tags
                          (-map 'rest)  ; Drop the numeric weight from each sublist
                          (-flatten))))
        (when (-contains? tag-list tag)
          (setq results table-tags))))))

;; ----------------------------------------------------------------------
;; Let's attempt to test our code and its theories.
;;
;; The function repeatedly selects items from a table randomly,
;; and returns a hash of the number of times each element was
;; selected ...

(defun rpgdm-tables-validate (&optional table-name iterations)
  "Return results randomly choosing many items from TABLE-NAME.
Calls `rpgdm-tables-choose' a number of ITERATIONS (defaults to 500)."
  (unless iterations (setq iterations 500))
  (unless table-name
    (setq table-name "test-subject")
    (puthash table-name (make-hash-table :test 'equal) rpgdm-tables)
    (setf (gethash "often" (gethash table-name rpgdm-tables))
          '(o1 o2 o3 o4 o5 o6 o7 o8 o9 o0))
    (setf (gethash "seldom" (gethash table-name rpgdm-tables))
          '(s1 s2 s3 s4 s5 s6 s7 s8 s9 s0))
    (setf (gethash "scarcely" (gethash table-name rpgdm-tables))
          '(l1 l2 l3 l4 l5 l6 l7 l8 l9 l0))
    (setf (gethash "rarely" (gethash table-name rpgdm-tables))
          '(r1 r2 r3 r4 r5 r6 r7 r8 r9 r0)))

  (let ((accumulator (make-hash-table :test 'equal)))
    (dotimes (i iterations accumulator)
      (let* ((item (rpgdm-tables-choose table-name))
             (item-name (first (s-split " :: " item))))
        (incf (gethash item-name accumulator 0))))
    accumulator))

;; Since we are randomly selecting items, even over large iterations, one can
;; see _scarcely_ appear almost as much as items labeled _often_. However, if we
;; first sort the data:
;;
;;     "o1" 35 "o2" 31 "o3" 38 "o4" 44 "o5" 43 ...
;;     "s1" 35 "s2" 38 "s3" 29 "s4" 28 "s5" 26 ...
;;     "l1" 26 "l2" 20 "l3" 19 "l4" 19 "l5" 26 ...
;;     "r1" 10 "r2" 7 "r3" 8 "r4" 5 "r5" 13 ...
;;
;; And then calculate the average of each _level_, we see that the items occur
;; as we would expect:
;;
;;     (/ (+ 35 31 38 44 43) 5)  -> 38
;;     (/ (+ 35 38 29 28 26) 5)  -> 31
;;     (/ (+ 26 20 19 19 26) 5)  -> 22
;;     (/ (+ 10  7  8  5 13) 5)  ->  8

(provide 'rpgdm-tables)
;;; rpgdm-tables.el ends here
