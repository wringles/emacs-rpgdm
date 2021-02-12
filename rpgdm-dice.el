;;; RPG-DM-HELPER --- Help for a Dungeon Master
;;
;; Author: Howard Abrams <howard@howardabrams.com>
;; Copyright © 2018, Howard Abrams, all rights reserved.
;; Created: 15 August 2018
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Functions to help a DM use an org-mode file as the basis of
;;  notes for an adventure.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; The basics of a dice roll is a random number from a given range. Note that if
;; we give a 6-sided die to the random number, we will end up with a range of 0
;; to 5, so we need to increase this value by 1.

(defun rpgdm--roll-die (sides)
  "Rolls a die of with SIDES."
  (1+ (random sides)))

;; ----------------------------------------------------------------------
;;   TESTING SUPPORT
;;
;; Unit Testing for random numbers is tricky, so what I will want to do is call
;; a function a number of times. A little function that `repeatedly' calls a
;; function with arguments and returns a list of the results should be helpful.

(defun repeatedly (fn args times)
  "Call a function, FN, with a list of arguments, ARGS, a number of TIMES.
Return a list of results."
  (let (value)
    (dotimes (number times value)
      (setq value (cons (apply fn args) value)))))

;; This function will run a large number of runs and verify that all dice rolls
;; fall between two ranges. This is completely accurate, as our dice rolls could
;; be in a smaller subset, so we also check to make sure that at least one roll
;; was at each end. This should be _good enough_.

(defun rpgdm--test-rolls (fn args min max)
  "Run function FN with ARGS to validate all results.
The numbers returned should be between MIN and MAX, with an
average value of AVG, if given."
  (let ((rolls (repeatedly fn args 1000)))
    (should (--some?  (= it min) rolls))
    (should (--some?  (= it max) rolls))
    (should (--every? (>= it min) rolls))
    (should (--every? (<= it max) rolls))))

(ert-deftest rpgdm--roll-expression-test ()
  "Simple test of my random number generator.
This really tests the `rpgdm--test-rolls' function."
  (dolist (test-data '((4 1 4)
                       (6 1 6)
                       (8 1 8)
                       (20 1 20)
                       (100 1 100)))
    (destructuring-bind (die lowest highest) test-data
      (rpgdm--test-rolls #'rpgdm--roll-die (list die) lowest highest))))

;; ----------------------------------------------------------------------

;; Now that we have a `rpgdm--roll-die' function that rolls a single die. How do
;; we want multiple dice rolls. Perhaps the results should be a list, so that we
;; can easily sum them, but still have the original results of each die roll.

(defun rpgdm--roll-dice (count sides)
  "Return a list of COUNT dice rolls where each die has SIDES."
  (let (value)
    (dotimes (_ count value)
      (setq value (cons (rpgdm--roll-die sides) value)))))

(ert-deftest rpgdm--roll-dice-test ()
  "Validate the `rpgdm--roll-dice' by making sure we get a list
of all die rolls, and that each number is within the range.
We can assume that `rpgdm--roll-dice' works."
  (let ((results (rpgdm--roll-dice 4 6)))
    (should (= (length results) 4))
    (should (--every? (>= it 1) results))
    (should (--every? (<= it 6) results))))

;; An RPG has checks that have multiple dice, plus a modifier (positive or
;; negative). When displaying the results, I want all the dice rolls displayed
;; differently from the modifier. Should we just assume the modifier is the
;; first or last number is the returned list?
;;
;; What if we have this function return a cons'd with the `car' be a list of the
;; rolls, and the `cdr' the modifier amount?

(defun rpgdm--roll (num-dice dice-type &optional modifier plus-minus)
  "Generate a random dice roll. Return tuple where `car' is a list of rolled
results, and the `cdr' is the modifier, see `rpgdm--sum'.

The NUM-DICE is the number of DICE-TYPE to roll. The PLUS-MINUS
is a string of either '+' or '-' to affect the results with the
MODIFIER amount. If PLUS-MINUS is nil, assume MODIFIER should
be added."
  (let* ((base-rolls (rpgdm--roll-dice num-dice dice-type)))
    (cond ((string= "-" plus-minus) (cons base-rolls (- modifier)))
          ((numberp modifier)       (cons base-rolls modifier))
          (t                        (cons base-rolls 0)))))

(defun rpgdm--sum (roll-combo)
  "Return a summation of the dice rolls in ROLL-COMBO tuple.
The tuple is a `cons'd structure where the `car' is a list of rolls,
and the `cdr' is a modifier, e.g. '((5 3 2 1 6) . 3)"
  (let ((rolls    (car roll-combo))
        (modifier (cdr roll-combo)))
    (+ (-sum rolls) modifier)))

(ert-deftest rpgdm--sum-test ()
  (should (= (rpgdm--sum '((1 6) . 3)) 10))
  (should (= (rpgdm--sum '((6) . -3)) 3))
  (should (= (rpgdm--sum '(() . 0)) 0)))

(defun rpgdm--test-roll-series (fn args min max)
  "Run function FN with ARGS to validate all results.
The numbers returned should be between MIN and MAX, with an
average value of AVG, if given."
  (let ((roll-sums (->> (repeatedly fn args 1000)
                        (-map 'rpgdm--sum))))
    ;; (should (--some?  (= it min) roll-sums))
    ;; (should (--some?  (= it max) roll-sums))
    (should (--every? (>= it min) roll-sums))
    (should (--every? (<= it max) roll-sums))))

(ert-deftest rpgdm--roll-test ()
  (let ((test-data '(((1 6) 1 6)
                     ((3 6 4) 7 22)
                     ((4 6 4 "-") 0 20))))
    (dolist (test-seq test-data)
      (destructuring-bind (dice-args lowest highest) test-seq
        (rpgdm--test-roll-series 'rpgdm--roll dice-args lowest highest)))))


;; For programmatic reasons, we need a quick way to roll dice and get a
;; numeric value.

(defun rpgdm-roll-sum (first &optional dice-type modifier)
  "Return a number value from rolling some dice.
The FIRST can be one of the following values:
 - A dice expression as a string, e.g. 2d4+2
 - A roll-combo tuple list
 - A single number of dice to roll (but this requires more values)

If FIRST is an integer, then DICE-TYPE is the number of dice sides.
MODIFIER, if given, is added to roll."
  (cond
   ((stringp first) (rpgdm--sum (rpgdm--roll-expression first)))
   ((listp first)   (rpgdm--sum first))
   (t               (rpgdm--sum (rpgdm--roll first dice-type modifier)))))


;; Now that we can roll a die with distinct numbers, let's now deal with dice
;; strings, e.g. 2d10+4. Can we have a regular expression that could identify
;; as well as pull apart the individual numbers?

(defvar rpgdm-roll-regexp
  (rx word-start
      (optional (group (one-or-more digit)))
      "d"
      (group (one-or-more digit))
      (optional
       (group (or "+" "-"))
       (group (one-or-more digit)))
      (optional ":"
                (group (one-or-more digit)))
      word-end)
  "A regular expression that matches a dice roll.")

;; See: ace-jump-search-candidate (re-query-string visual-area-list)

(defun rpgdm-forward-roll (count)
  "Move the point to the next COUNT of a dice roll.

Note: This moves the point to the _beginning_ of what is
considered the dice roll description, which could include any of
the following:

  - d8
  - 2d6
  - 1d12+5
  - d20-4"
  (interactive "p")
  (when (looking-at-p rpgdm-roll-regexp)
    (re-search-forward rpgdm-roll-regexp))
  (dotimes (repeat count)
    (re-search-forward rpgdm-roll-regexp))
  (goto-char (match-beginning 0)))

(defun rpgdm-dice-format-string (str)
  "Replace all dice expressions in STR with a dice roll results."
  (while (string-match rpgdm-roll-regexp str)
    (replace-regexp-in-string (concat rpgdm-roll-regexp "'") 'rpgdm-roll-sum str)))

;; Practice:  somed8 d4 2d8 3d6+2

(defun rpgdm--roll-expression (expression)
  "Return dice roll of EXPRESSION as a string, e.g. 2d6+3."
  (if (string-match rpgdm-roll-regexp expression)
      (let* ((num-dice-s  (or (match-string 1 expression) "1"))
             (num-dice    (string-to-number num-dice-s))
             (dice-type-s (or (match-string 2 expression) "20"))
             (dice-type   (string-to-number dice-type-s))
             (plus-minus  (or (match-string 3 expression) "+"))
             (modifier-s  (or (match-string 4 expression) "0"))
             (modifier    (string-to-number modifier-s)))
        (rpgdm--roll num-dice dice-type modifier plus-minus))
    (rpgdm--roll 1 20)))

(ert-deftest rpgdm--roll-expression-test ()
  (let ((test-cases '(("d6" 1 6)
                      ("DC" 1 20)
                      ("2d12" 2 24)
                      ("3d6+2" 5 20))))
    (dolist (test-data test-cases)
      (destructuring-bind (dice-expression lowest highest) test-data
        (rpgdm--test-roll-series 'rpgdm--roll-expression (list dice-expression) lowest highest)))))


(defun rpgdm--display-roll (roll-combo &optional expression)
  "Convert a ROLL-COMBO.results into a suitable string.
The format for a roll combo is described with `rpgdm--sum' function.
The EXPRESSION is a string that may have generated the roll combo."
  (let ((answer (rpgdm--sum roll-combo))
        (die-rolls (car roll-combo))
        (modifier (cdr roll-combo)))
    (rpgdm--display-roll-parts answer die-rolls modifier expression)))

(defun rpgdm--display-roll-parts (answer die-rolls modifier &optional expression)
  "Render parameters into a suitable string.
The ANSWER is probably the sum expression of our dice rolls,
rendered brightly. And DIE-ROLLS is a list of the die rolls.
MODIFIER is positive or negative number. The EXPRESSION is a
string that may have generated the roll combo."
  (let* ((sum-str (propertize (number-to-string answer) 'face 'alert-moderate-face))
         (die-str (cond ((and (= (length die-rolls) 1)
                              (= modifier 0))            "")
                        ((= (length die-rolls) 1)       (format " ... %d" (car die-rolls)))
                        (t                              (format " ... %s" die-rolls))))
         (mod-str (cond ((> modifier 0)  (format " +%d" modifier))
                        ((< modifier 0)  (format " %d" modifier))
                        (t               "")))
         (exp-str (if expression
                      (format " | %s" expression)
                    "")))
    (format "%s%s%s%s" sum-str die-str mod-str exp-str)))

(ert-deftest rpgdm--display-roll-test ()
  (should (equal (rpgdm--display-roll '((1 2 3) . 0)) "6 ... (1 2 3)"))
  (should (equal (rpgdm--display-roll '((1 2 3) . 4)) "10 ... (1 2 3) +4"))
  (should (equal (rpgdm--display-roll '((1 2 3) . -4)) "2 ... (1 2 3) -4"))
  (should (equal (rpgdm--display-roll '((2) . 4)) "6 ... 2 +4"))
  (should (equal (rpgdm--display-roll '((2) . 0)) "2"))
  (should (equal (rpgdm--display-roll '((1 2 3) . 4) "3d6+4") "10 ... (1 2 3) +4 | 3d6+4")))

(defun rpgdm-roll (expression)
  "Generate a random number based on a given dice roll EXPRESSION.
Unless the point is on a dice roll description, e.g 2d12+3."
  (interactive (list (if (looking-at rpgdm-roll-regexp)
                         (match-string-no-properties 0)
                       (read-string "Dice Expression: "))))
  (let ((roll-results (rpgdm--roll-expression expression)))
    (rpgdm-message "Rolled: %s" (rpgdm--display-roll roll-results expression))))


;; ----------------------------------------------------------------------
;;    ADVANTAGE and DISADVANTAGE ROLLS
;; ----------------------------------------------------------------------

(defun rpgdm--roll-with-choice (choose-fn modifier &optional plus-minus)
  "Roll a d20 but choose the results based on the CHOOSE-FN function.
This is really a helper function for rolling with advantage or
disadvantage. The results are added to the MODIFIER and PLUS-MINUS,
and the entire thing is formatted with `rpgdm--display-roll-parts'."
  (let* ((rolls (rpgdm--roll 2 20 modifier plus-minus))
         (die-rolls (car rolls))
         (modifier (cdr rolls))
         (answer (+ (apply choose-fn die-rolls) modifier)))
    (rpgdm--display-roll-parts answer die-rolls modifier)))

(defun rpgdm-roll-advantage (modifier &optional plus-minus)
  "Roll a d20 with advantage (rolling twice taking the higher).
If looking at a dice expression, use it for MODIFIER (the
PLUS-MINUS string from the regular expression,`rpgdm-roll-regexp'),
otherwise, prompt for the modifier. Results are displayed."
  (interactive (list (if (looking-at rpgdm-roll-regexp)
                         (match-string-no-properties 0)
                       (read-number "Advantage roll with modifier: "))))
  (rpgdm-message "Rolled with Advantage: %s"
           (rpgdm--roll-with-choice 'max modifier plus-minus)))

(defun rpgdm-roll-disadvantage (modifier &optional plus-minus)
  "Roll a d20 with disadvantage (rolling twice taking the lower).
If looking at a dice expression, use it for MODIFIER (the
PLUS-MINUS string from the regular expression,`rpgdm-roll-regexp'),
otherwise, prompt for the modifier. Results are displayed."
  (interactive (list (if (looking-at rpgdm-roll-regexp)
                         (match-string-no-properties 0)
                       (read-number "Disadvantage roll with modifier: "))))
  (rpgdm-message "Rolled with Disadvantage: %s"
           (rpgdm--roll-with-choice 'min modifier plus-minus)))


(provide 'rpgdm-dice)
;;; rpgdm-dice.el ends here
