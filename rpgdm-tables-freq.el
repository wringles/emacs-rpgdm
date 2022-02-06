;;; rpgdm-tables-freq.el --- Rolling dice for choosing items from Tables -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Howard X. Abrams
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams <howard.abrams@gmail.com>
;; Created: February  5, 2021
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;     This file contains the source code, but the concept behind what I'm
;;     calling random frequency tables is a bit complex, so I recommend looking
;;     at the original file in `docs/rpgdm-tables-freq.org'.

(defun rpgdm-tables-freq-table? ()
  "Return non-nil if current buffer contains a frequency table"
  (goto-char (point-min))
  (re-search-forward rpgdm-tables--line-parse nil nil)
  (match-string 2))

(defun rpgdm-tables--parse-as-freq-table ()
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
    (rpgdm-tables--merge-frequencies results)))

(defun rpgdm-tables--merge-frequencies (table)
  "Combine the values of equivalent table-tags in TABLE.
A table, read as a hash table, may have similar, but equal tags.
For instance, `veryrare' and `very-rare' are the same."
  (let* ((table-tags (rpgdm-tables--which-tag-group table))
         (tags (-map 'rest table-tags)))  ; Ignore all the numbers
    (dolist (subtag-list tags)
      (unless (= 1 (length subtag-list))
        (let ((keeper (first subtag-list)))
          (dolist (tag (rest subtag-list))
            (puthash keeper (append (gethash keeper table)
                                    (gethash tag table)) table)
            (remhash tag table)))))
    table))

(defconst rpgdm-tables-tag-groups
  '(((12 "common")
     (7 "uncommon")
     (4 "rare")
     (2 "veryrare" "very-rare" "very rare")
     (1 "legendary"))

    ((4 "often")
     (3 "seldom" "sometimes")
     (2 "scarcely" "scarce" "hardly ever")
     (1 "rarely"))))

(defun rpgdm-tables--choose-freq-table (table)
  "Select item from a hash TABLE.
Note that tables stored in a hash table have weight keys and a list
of items associated with that weight."
  (let* ((table-tags (rpgdm-tables--table-distribution table))
         (tag (rpgdm-tables--choose-tag table-tags)))
    (seq-random-elt (gethash tag table))))


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
             (item-name (first (split-string item " :: "))))
        (incf (gethash item-name accumulator 0))))
    accumulator))

(provide 'rpgdm-tables-freq)
;;; rpgdm-tables-freq.el ends here
