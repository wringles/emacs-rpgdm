;;; rpgdm.el --- Support utilities for the RPG Game Master -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Howard X. Abrams
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams
;; Created: January  4, 2021
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;     This package includes all the help and support I can think for the Game
;;     Master running a role-playing game.
;;
;;     This include a minor mode, `rpgdm' that adds a few keybindings useful
;;     with either org-mode or markdown-formatted files.
;;
;;; Code:

(require 'cl)
(require 'dash)
(require 'hydra)
(require 's)

(require 'ert)

(require 'rpgdm-dice)
(require 'rpgdm-screen)
(require 'rpgdm-tables)


(defvar rpgdm-base
  (seq-find (lambda (elt) (string-match "rpgdm" elt)) load-path (getenv "HOME"))
  "Default directory to look for supporting data, like tables and charts.")

(define-minor-mode rpgdm-mode
  "Minor mode for layering role-playing game master functions over your notes."
  :lighter " D&D"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<f6>") 'hydra-rpgdm/body)
            map))

(defhydra hydra-rpgdm (:color pink :hint nil)
  "
    ^Dice^                              ^Tables^          ^Checks^                        ^Moving^          ^Messages^
 -----------------------------------------------------------------------------------------------------------------
    _d_: Roll Dice / _D_: Reroll Dice  _r_: Dashboard    _s_: d20 Skill  _m_: Moderate      _o_: Links        ⌘-l: Last Results
    _z_: Flip a coin _O_: Oracle roll  _t_: Load Tables  _e_: Easy check _h_: Hard check  _J_/_K_: Page up/dn   ⌘-k: ↑ Previous
    _b_: Previous  _f_: Next Dice Expr _c_: Choose Item  _v_: Difficult  _i_: Impossible  _N_/_W_: Narrow/Widen ⌘-j: ↓ Next
    _a_/_A_: Advantage/Disadvantage    _N_: Show NPC "
  ("d" rpgdm-roll)              ("D" rpgdm-roll-again)
  ("f" rpgdm-forward-roll)      ("b" rpgdm-forward-roll)
  ("a" rpgdm-roll-advantage)    ("A" rpgdm-roll-disadvantage)
  ("z" rpgdm-yes-and-50/50)     ("O" rpgdm-oracle)
  ("s" rpgdm-skill-check)       ("i" rpgdm-skill-check-impossible)
  ("e" rpgdm-skill-check-easy)  ("m" rpgdm-skill-check-moderate)
  ("h" rpgdm-skill-check-hard)  ("v" rpgdm-skill-check-difficult)

  ("t" rpgdm-tables-load)       ("c" rpgdm-tables-choose) ("C" rpgdm-tables-choose :color blue)
  ("r" rpgdm-screen-show)       ("R" rpgdm-quick-close)
  ("o" ace-link)                ("N" org-narrow-to-subtree)   ("W" widen)
  ("K" scroll-down)             ("J" scroll-up)

  ("N" rpgdm-npc)

  ("y" rpgdm-paste-last-message)

  ("C-m" rpgdm-last-results)
  ("C-n" rpgdm-last-results-next) ("C-p" rpgdm-last-results-previous)
  ("s-l" rpgdm-last-results)
  ("s-j" rpgdm-last-results-next) ("s-k" rpgdm-last-results-previous)

  ("q" nil "quit") ("<f12>" nil))


(defvar rpgdm-last-results (make-ring 10)
  "The results from calls to `rpgdm-screen-' functions are stored here.")

(defvar rpgdm-last-results-ptr 0
  "Keeps track of where we are in the message display ring.
Each call to `rpgdm-last-results' resets this to 0.")

(defun rpgdm-message (format-string &rest args)
  "Replace `messasge' function allowing it to be re-displayed.
The FORMAT-STRING is a standard string for the `format' function,
and ARGS are substitued values."
  (let ((message (apply 'format format-string args)))
    (ring-insert rpgdm-last-results message)
    (rpgdm-last-results)))

(defun rpgdm-last-results ()
  "Display results from the last call to a `rpgdm-message' function."
  (interactive)
  (setq rpgdm-last-results-ptr 0)
  (message (ring-ref rpgdm-last-results rpgdm-last-results-ptr)))

(defun rpgdm-last-results-previous ()
  "Display results from an earlier call to `rpgdm-message'."
  (interactive)
  (incf rpgdm-last-results-ptr)
  (when (>= rpgdm-last-results-ptr (ring-length rpgdm-last-results))
    (setq rpgdm-last-results-ptr 0))
  (message "%d> %s" rpgdm-last-results-ptr (ring-ref rpgdm-last-results rpgdm-last-results-ptr)))

(defun rpgdm-last-results-next ()
  "Display results from an later call to `rpgdm-message'.
Meant to be used with `rpgdm-last-results-previous'."
  (interactive)
  (when (> rpgdm-last-results-ptr 0)
    (decf rpgdm-last-results-ptr))
  (message "%d> %s" rpgdm-last-results-ptr (ring-ref rpgdm-last-results rpgdm-last-results-ptr)))

(defun rpgdm-paste-last-message ()
  "Yank, e.g. paste, the last displayed message."
  (interactive)
  (insert (rpgdm-last-results)))

(ert-deftest rpgdm-last-results-test ()
  (progn
    (setq rpgdm-last-results (make-ring 10))
    (rpgdm-message "First in, so this is the oldest")
    (rpgdm-message "Something or other")
    (rpgdm-message "Almost the newest")
    (rpgdm-message "Newest"))

  (should (equal "Newest" (rpgdm-last-results)))
  (should (equal "1> Almost the newest" (rpgdm-last-results-previous)))
  (should (equal "2> Something other" (rpgdm-last-results-previous)))
  (should (equal "1> Almost the newest" (rpgdm-last-results-next)))
  (should (equal "0> Almost the newest" (rpgdm-last-results-next)))
  (should (equal "0> Almost the newest" (rpgdm-last-results-next))))

(defvar rpgdm-oracle-mod 0 "Cummulative skew to create more tension.")

(defun rpgdm-oracle ()
  "Return a good/fair/bad message for GM-less result.
The algorithm here comes from Victor's GM-less Oracle game, see
https://cursenightgames.itch.io/victors-gm-less-oracle

This results of this idea mimic a bell curve, but creates
tension, for with more good outcomes, a bad outcome is more
likely. How this works in game play vs. a standard bell curve is
not clear."
  (interactive)
  (let* ((rolled  (rpgdm--roll-die 6))
         (outcome (+ rolled rpgdm-oracle-mod))
         (diffmsg (if (= rolled outcome) "" ; empty string if true
                    (format " with mod of %d" rpgdm-oracle-mod)))
         (results (cond ((<= outcome 1) "Best outcome")
                        ((<= outcome 3) (progn
                                          (setq rpgdm-oracle-mod (1+ rpgdm-oracle-mod))
                                          "Best outcome"))
                        ((<= outcome 5) "Middling outcome")
                        (t             (progn
                                         (setq rpgdm-oracle-mod 0)
                                         "Worst outcome")))))
    (rpgdm-message "Oracle says: %s (Rolled %d%s ... Now mod is %d)"
                   results rolled diffmsg rpgdm-oracle-mod)))

(defun rpgdm-yes-and-50/50 ()
  "Add spice to your 50/50 events (luck) with Yes/No+complications.

The Freeform Universal RPG has the idea that you could succeed or
fail, but have extra complications or extra bonuses. This returns
one of six answers with equal frequency:

  - No, and  ... in other words, no luck, plus a minor complication.
  - No.      ... Nope, no luck at this time.
  - No, but  ... in other words, no luck, but something else good.
  - Yes, but ... you got what you wanted, but with a complication.
  - Yes.     ... Yup, luck is on your side.
  - Yes, and ... Yes, plus you get a little something-something.

https://www.drivethrurpg.com/product/89534/FU-The-Freeform-Universal-RPG-Classic-rules"
  (interactive)
  (let* ((rolled (rpgdm--roll-die 6))
         (results (cond ((= rolled 1)  "No, and... (fails badly that you add a complication)")
                        ((= rolled 2)  "No.")
                        ((= rolled 3)  "No, but... (fails, but add a little bonus or consolation prize)")
                        ((= rolled 4)  "Yes, but... (succeeds, but add a complication or caveat)")
                        ((= rolled 5)  "Yes.")
                        (t             "Yes, and... (succeeds, plus add a litle extra something-something)"))))
    (rpgdm-message "Luck results: %s" results)))


;; ----------------------------------------------------------------------
;;    SKILL CHECKS
;; ----------------------------------------------------------------------
;;  I would like to have a function that

(defun rpgdm--skill-level-dice (number-of-dice)
  "Return a random skill challenge level.
The formula is based on the NUMBER-OF-DICE. According to the
Players Handbook in Dungeons and Dragons, we have this table
to determine difficulty skill check levels:

  - Very easy	5
  - Easy	10
  - Medium	15
  - Hard	20
  - Very hard	25
  - Nearly impossible	30

But I read somewhere that you could roll some 6 sided die to help
add a bit of randomness to the leve setting. Essentially, roll
the 6d and add 7.

    Easy -- Die: 1 Lowest: 8  Highest: 13  Average: 10
    Medium -- Die: 2 Lowest: 9  Highest: 19  Average: 14
    Hard -- Die: 3 Lowest: 10  Highest: 25  Average: 17
    Very hard -- Die: 4 Lowest: 11  Highest: 30  Average: 20
    Nearly Impossible -- Die: 5 Lowest: 14  Highest: 35  Average: 24"
  (rpgdm--sum
   (rpgdm--roll number-of-dice 6 7)))

;; Let's verify my assumptions:
;;   (dolist (die '(0 1 2 3 4 5))
;;     (let* ((rolls (repeatedly 'rpgdm--skill-level-dice (list die) 1000))
;;            (highest (apply 'max rolls))
;;            (lowest  (apply 'min rolls))
;;            (average (/ (-sum rolls) 1000)))
;;       (message
;;        (format "Die: %d Lowest: %d  Highest: %d  Average: %d\n" die lowest highest average))))

(defun rpgdm--skill-level (target)
  "Return a skill challenge level by Interpreting TARGET.
This parameter can be a symbol or string for 'easy', 'hard', etc.
Or it can be an actual number."
  (when (symbolp target)
    (setq target (symbol-name target)))
  (cond ((string-prefix-p target "trivial" t)    5)
        ((string-prefix-p target "easy" t)       (rpgdm--skill-level-dice 1))
        ((string-prefix-p target "moderate" t)   (rpgdm--skill-level-dice 2))
        ((string-prefix-p target "medium" t)     (rpgdm--skill-level-dice 2))
        ((string-prefix-p target "hard" t)       (rpgdm--skill-level-dice 3))
        ((string-prefix-p target "difficult" t)  (rpgdm--skill-level-dice 4))
        ((string-prefix-p target "very hard" t)  (rpgdm--skill-level-dice 4))
        ((string-prefix-p target "impossible" t) (rpgdm--skill-level-dice 5))
        ((numberp target)                        target)
        (t                                       (max (string-to-number target) 12))))

(defun rpgdm--yes-and (target rolled-results)
  "Instead of returning a pass/fail, return 'Yes, but' strings.

The Freeform Universal RPG has the idea that you could succeed or
fail, but have extra complications or extra bonuses based on how
high/low you pass/fail. I have expanded the idea with a d20, so
given a TARGET number, like '12', and the ROLLED-RESULTS from the
player, this returns a string based on a table.

https://www.drivethrurpg.com/product/89534/FU-The-Freeform-Universal-RPG-Classic-rules"
  (cond ((< rolled-results (- target 7))  "No, and... !!")
        ((< rolled-results (- target 3))  "No.")
        ((< rolled-results target)        "No, but...")
        ((< rolled-results (+ target 3))  "Yes, but...")
        ((< rolled-results (+ target 7))  "Yes.")
        (t                                "Yes, and... !!")))

(ert-deftest rpgdm--yes-and-test ()
  (should (equal (rpgdm--yes-and 10 1) "No, and..."))
  (should (equal (rpgdm--yes-and 10 2) "No, and..."))
  (should (equal (rpgdm--yes-and 10 3) "No."))
  (should (equal (rpgdm--yes-and 10 4) "No."))
  (should (equal (rpgdm--yes-and 10 5) "No."))
  (should (equal (rpgdm--yes-and 10 6) "No."))
  (should (equal (rpgdm--yes-and 10 7) "No, but..."))
  (should (equal (rpgdm--yes-and 10 8) "No, but..."))
  (should (equal (rpgdm--yes-and 10 9) "No, but..."))
  (should (equal (rpgdm--yes-and 10 10) "Yes, but..."))
  (should (equal (rpgdm--yes-and 10 11) "Yes, but..."))
  (should (equal (rpgdm--yes-and 10 12) "Yes, but..."))
  (should (equal (rpgdm--yes-and 10 13) "Yes."))
  (should (equal (rpgdm--yes-and 10 14) "Yes."))
  (should (equal (rpgdm--yes-and 10 15) "Yes."))
  (should (equal (rpgdm--yes-and 10 16) "Yes."))
  (should (equal (rpgdm--yes-and 10 17) "Yes, and...")))

(defun rpgdm-skill-check (target rolled-results &optional label)
  "Given a TARGET skill check, and ROLLED-RESULTS, return pass/fail.
The string can return a bit of complications, from `rpgdm--yes-and'.
The LABEL will be append to the message, and used form other calls."
  (interactive (list (completing-read "Target Level: "
                                      '(Trivial Easy Moderate Hard Difficult Impossible))
                     (read-number "Rolled Results: ")))
  (rpgdm-message "%s Skill Check: %s"
                 (capitalize target)
                 (rpgdm--yes-and (rpgdm--skill-level (downcase target)) rolled-results)))

(defun rpgdm-skill-check-easy (rolled-results)
  "Return an embellished pass/fail from ROLLED-RESULTS for an easy skill check."
  (interactive "nEasy Check- Rolled Results: ")
  (rpgdm-skill-check (rpgdm--skill-level 'easy) rolled-results))

(defun rpgdm-skill-check-moderate (rolled-results)
  "Return an embellished pass/fail from ROLLED-RESULTS for a moderately-difficult skill check."
  (interactive "nModerate Check- Rolled Results: ")
  (rpgdm-skill-check (rpgdm--skill-level 'medium) rolled-results))

(defun rpgdm-skill-check-hard (rolled-results)
  "Return an embellished pass/fail from ROLLED-RESULTS for a hard skill check."
  (interactive "nHard Check- Rolled Results: ")
  (rpgdm-skill-check (rpgdm--skill-level 'hard) rolled-results))

(defun rpgdm-skill-check-difficult (rolled-results)
  "Return an embellished pass/fail from ROLLED-RESULTS for a difficult skill check."
  (interactive "nVery Hard Check- Rolled Results: ")
  (rpgdm-skill-check (rpgdm--skill-level 'difficult) rolled-results))

(defun rpgdm-skill-check-impossible (rolled-results)
  "Return an embellished pass/fail from ROLLED-RESULTS for an almost impossible skill check."
  (interactive "nImpossible Check- Rolled Results: ")
  (rpgdm-skill-check (rpgdm--skill-level 'impossible) rolled-results))

(provide 'rpgdm)
;;; rpgdm.el ends here
