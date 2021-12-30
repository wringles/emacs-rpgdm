;;; rpgdm-mythic.org --- Functions to help when playing Mythic RPG. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Howard X. Abrams
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams <howard.abrams@gmail.com>
;; Created: 9 August 2021
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;    This source code is literally extracted from an essay about creating an
;;    user interface to the Mythic RPG game system. See the book,
;;    http://wordmillgames.com/mythic-rpg.html
;;
;;; Code:

(map! :leader (:prefix-map ("a" . "abrams")
                (:prefix-map ("r" . "rpg-dm")
                  :desc "table choice"        "c" 'rpgdm-tables-choose
                  :desc "roll the dice"       "r" 'rpgdm-roll
                  :desc "best/middling/worse" "z" 'rpgdm-oracle
                  :desc "flip coin, and..."   "a" 'rpgdm-yes-and-50/50
                  :desc "new adventure"       "N" 'rpgdm-mythic-new-adventure
                  :desc "new scene"           "n" 'rpgdm-mythic-new-scene
                  :desc "what are the odds?"  "o" 'rpgdm-mythic-fate-odds
                  :desc "skill challenge"     "f" 'rpgdm-mythic-fate-challenge)))

(global-set-key (kbd "<f19> c") 'rpgdm-tables-choose)
(global-set-key (kbd "<f19> r") 'rpgdm-roll)
(global-set-key (kbd "<f19> z") 'rpgdm-oracle)
(global-set-key (kbd "<f19> a") 'rpgdm-yes-and-50/50)
(global-set-key (kbd "<f19> N") 'rpgdm-mythic-new-adventure)
(global-set-key (kbd "<f19> n") 'rpgdm-mythic-new-scene)
(global-set-key (kbd "<f19> o") 'rpgdm-mythic-fate-odds)
(global-set-key (kbd "<f19> f") 'rpgdm-mythic-fate-challenge)

(global-set-key (kbd "<f15>") 'rpgdm-mythic-fate-odds)

(defun rpgdm-mythic-rank-level (&optional rank-type)
  "Query user and return a numeric 'rank' level.
This number is from -5 to 7, where 0 is a average."
  (let* ((prompt (format "Choose a %sMythic Rank:
  t) Trivial  m) Miniscule    w) Weak        l) Low      b) Below Average  a) Average  A) Above Average
  H) High     E) Exceptional  I) Incredible  W) Awesome  S) Superhuman     V) Very Superhuman "
                         (or rank-type "")))
         (ascii (read-char (rpgdm--prompt-emphasis prompt))))
    (cond
     ((eq ascii ?t)                                  -5) ; t -> trivial
     ((eq ascii ?m)                                  -4) ; m -> miniscule
     ((eq ascii ?w)                                  -3) ; w -> weak                                      -
     ((eq ascii ?l)                                  -2) ; l -> low                                       - terrible
     ((or (eq ascii ?b) (eq ascii ?-))               -1) ; b -> below average (will accept a negative)    - poor
     ((or (eq ascii ?A) (eq ascii ?1) (eq ascii ?+))  1) ; A -> above average                             - average
     ((or (eq ascii ?H) (eq ascii ?h) (eq ascii ?2))  2) ; h -> high                                      - Fair
     ((or (eq ascii ?E) (eq ascii ?e) (eq ascii ?3))  3) ; e -> exceptional                               - Good
     ((or (eq ascii ?I) (eq ascii ?i) (eq ascii ?4))  4) ; i -> incredible                                - Great
     ((or (eq ascii ?W) (eq ascii ?5))                5) ; W -> awesome                                   - Superb
     ((or (eq ascii ?S) (eq ascii ?6))                6) ; s -> superhuman                                - Fantastic
     ((or (eq ascii ?V) (eq ascii ?7))                7) ; S -> superhuman+                               - Epic
     (t                                               0)))) ; * -> Average                                - mediocre

(defun rpgdm-mythic-odds-level ()
  "Query user and return a numeric 'odds' level.
This number is from -4 to 6, where 0 is a 50/50."
  (let ((ascii (read-char (rpgdm--prompt-emphasis "What is the likelihood of your question?
  i) Impossible  n) No way  v) Very unlikely u) Unlikely   h) 50/50
  M) Maybe       L) Likely  P) Probably      N) Near Sure  S) Sure thing  A) Absolutely"))))
    (cond
     ((eq ascii ?i)                                   -4) ; i -> impossible
     ((eq ascii ?n)                                   -3) ; n -> no way
     ((eq ascii ?v)                                   -2) ; v -> very unlikely
     ((or (eq ascii ?u)               (eq ascii ?-))  -1) ; u -> unlikely
     ((or (eq ascii ?M) (eq ascii ?m) (eq ascii ?1))   1) ; M -> maybe
     ((or (eq ascii ?L) (eq ascii ?l) (eq ascii ?2))   2) ; L -> likely
     ((or (eq ascii ?P) (eq ascii ?p) (eq ascii ?3))   3) ; P -> probably
     ((or (eq ascii ?N) (eq ascii ?s) (eq ascii ?4))   4) ; N -> near sure
     ((or (eq ascii ?S) (eq ascii ?t) (eq ascii ?5))   5) ; S -> sure thing
     ((or (eq ascii ?A) (eq ascii ?a)
          (eq ascii ?Y) (eq ascii ?y) (eq ascii ?6))   6) ; A -> absolutely
     (t                                                0)))) ; * -> 50/50

(defun rpgdm-mythic-chaos-level ()
  "Query user and return a numeric 'chaos' level.
Where `1' means a stable environment where most yes/no questions
are no, and `9' is chaotic, and more often responds with yes.
Return number is from -4 to 4, where 0 is normal."
  (let* ((prompt (format "What is the Chaos level (1-9)?
Where 1 is very stable (more noes), and 9 is chaotic (more yeses), and <RET> selects the current value, %d"
                         rpgdm-mythic-current-chaos-level))
        (ascii (read-char prompt)))
    (cond
     ((eq ascii ?9) -4)
     ((eq ascii ?8) -3)
     ((eq ascii ?7) -2)
     ((eq ascii ?6) -1)
     ((eq ascii ?5)  0)
     ((eq ascii ?4)  1)
     ((eq ascii ?3)  2)
     ((eq ascii ?2)  3)
     ((eq ascii ?1)  4)
     (t              (- rpgdm-mythic-current-chaos-level 5)))))

(defun rpgdm--prompt-emphasis (message)
  "Add emphasizing properties to the keystroke prompts in MESSAGE."
  (let ((start 0)
        (re (rx bow (one-or-more (not space)) ") ")))
    (while (string-match re message start)
      (let* ((key-start (match-beginning 0))
             (key-end   (1+ key-start))
             (par-start  key-end)
             (par-end   (1+ par-start)))
        (put-text-property key-start key-end 'face '(:foreground "green") message)
        (put-text-property par-start par-end 'face '(:foreground "#666666") message))
      (setq start (match-end 0))))
  message)

(defun rpgdm-mythic-fate-challenge (acting-rank acting-modifier difficulty-rank)
  "Request a challange on the tables of fate.
Send a message of the results of rolling a d100 on the Mythic
Fate Chart. The ACTING-RANK and DIFFICULTY-RANK are numeric
values from -5 to 7 (with 0 being average)."
  (interactive (list (rpgdm-mythic-rank-level "Acting ")
                     (read-number "Actiing modifier? " 0)
                     (rpgdm-mythic-rank-level "Difficulty ")))
  (rpgdm-mythic-fate-chart "Challenge " (+ acting-rank acting-modifier) difficulty-rank))

(defun rpgdm-mythic-fate-odds (odds chaos-level)
  "Request a results of what are the odds on the tables of fate.
Send a message of the results of rolling a d100 on the Mythic
Fate Chart. The ODDS is the likelihood of something, and the
CHAOS-LEVEL is a numeric values about how likely yes answer
happen."
  (interactive (list (rpgdm-mythic-odds-level)
                     (rpgdm-mythic-chaos-level)))
  (rpgdm-mythic-fate-chart "Odds " odds chaos-level))

(defun rpgdm-mythic-fate-chart (chart-type x-rank y-rank)
  "Return a colorized message of rolling dice against the Fate Chart.
Use the X-RANK and Y-RANK as indexes in the Mythic RPG Fate
chart (see `rpgdm-mythic--fate-chart'), and format the collected
messages."
  (let* ((range   (rpgdm-mythic--fate-chart x-rank y-rank))
         (roll    (rpgdm--roll-die 100))
         (results (rpgdm-mythic--result-message range roll))
         (event?  (rpgdm-mythic--result-event-p range roll))
         (message (format "Mythic %s- %d < %d < %d :: %d ... %s %s"
                   chart-type (first range) (second range) (third range) roll results event?)))
    (rpgdm-message (rpgdm-mythic--fate-chart-emphasize message))))

(let ((fate-table-values '((50 25 10 5 5 0 0 -20 -20 -40 -40 -55 -65) (75 50 25 15 10 5 5 0 0 -20 -20 -35 -45) (90 75 50 35 25 15 10 5 5 0 0 -15 -25) (95 85 65 50 45 25 15 10 5 5 5 5 -15) (100 90 75 55 50 35 20 15 10 5 5 0 -10) (105 95 85 75 65 50 35 25 15 10 10 5 -5) (110 95 90 85 80 65 50 45 25 20 15 5 0) (115 100 95 90 85 75 55 50 35 25 20 10 5) (120 105 95 95 90 85 75 65 50 45 35 15 5) (125 115 100 95 95 90 80 75 55 50 45 20 10) (130 125 110 95 95 90 85 80 65 55 50 25 10) (150 145 130 100 100 95 95 90 85 80 75 50 25) (170 165 150 120 120 100 100 95 95 90 90 75 50))))
(defvar rpgdm-mythic-fate-table fate-table-values
  "Only contains the medium boundary values of the Mythic Fate Chart.")
)

(defun rpgdm-mythic--fate-boundary (acting-rank difficulty-rank)
  "Return the boundary value for a fate contest. The ACTING-RANK
and DIFFICULTY-RANK are numeric values from -5 to 7 that
correspond to the rows and colums of the Fate Chart from the
Mythic RPG."
  (nth (+ difficulty-rank 5)
       (nth (+ acting-rank 5) rpgdm-mythic-fate-table)))

(defun rpgdm-mythic--fate-chart (acting-rank difficulty-rank)
  "Return a list of the lower, medium and upper boundaries of the Fate Chart.
The ACTING-RANK and DIFFICULTY-RANK are numeric values from -5 to
7 that correspond to the rows and colums of the Fate Chart, for
instance, a value of _weak_ on the chart is a -3, and a value of
_incredible_ is a 4."
  (let* ((medium (rpgdm-mythic--fate-boundary acting-rank difficulty-rank))
         (lower  (if (> medium 0)   (/ medium 5)         0))
         (upper  (if (< medium 100)
                     (- 100 (/ (- 100 medium) 5)) 100)))
    (list lower medium upper)))

(ert-deftest rpgdm-mythic--fate-chart-test ()
  (should (equal (rpgdm-mythic--fate-chart 0 0)   '(10 50 90)))
  (should (equal (rpgdm-mythic--fate-chart 7 7)   '(10 50 90)))
  (should (equal (rpgdm-mythic--fate-chart -5 -5) '(10 50 90)))
  (should (equal (rpgdm-mythic--fate-chart -5 7)  '(0 -65 67)))
  (should (equal (rpgdm-mythic--fate-chart 7 -5)  '(34 170 100)))
  (should (equal (rpgdm-mythic--fate-chart -1 2)  '(3 15 83))))

(defun rpgdm-mythic--result-message (chart-range die-roll)
  "Return result message of a DIE-ROLL within the three numbers from the CHART-RANGE."
  (cond
   ((<= die-roll (first chart-range))  "Exceptional yes")
   ((<= die-roll (second chart-range)) "Yes")
   ((<= die-roll (third chart-range))  "No")
   (t                                  "Exceptional no")))

(defun rpgdm-mythic--result-event-p (chart-range die-roll)
  "Return a random event message of a DIE-ROLL within the yes values of the CHART-RANGE."
  (let ((tens (/ die-roll 10))
        (ones (mod die-roll 10)))
    (when (and (= ones tens)
               (<= die-roll (second chart-range)))
      " <Random Event>")))

(ert-deftest rpgdm-mythic--result-event-p-test ()
  (should (rpgdm-mythic--result-event-p '(10 50 90) 11))
  (should (rpgdm-mythic--result-event-p '(10 50 90) 44))
  (should (not (rpgdm-mythic--result-event-p '(10 50 90) 88)))
  (should (not (rpgdm-mythic--result-event-p '(10 50 90) 13))))

(defun rpgdm-mythic--fate-chart-emphasize (message)
  "We make certain assumption about the format of the message."
  (let ((roll-re (rx ":: " (group (one-or-more digit))))
        (main-re (rx "< " (group (one-or-more digit)) " <"))
        (punc-re (rx "..."))
        (rest-re (rx "... " (group (one-or-more any)))))
    (string-match roll-re message)
    (put-text-property (match-beginning 1) (match-end 1) 'face '(:foreground "yellow") message)
    (string-match main-re message)
    (put-text-property (match-beginning 1) (match-end 1) 'face '(:foreground "green") message)
    (string-match rest-re message)
    (put-text-property (match-beginning 1) (match-end 1) 'face '(:foreground "white") message)
    (string-match punc-re message)
    (put-text-property (match-beginning 0) (match-end 0) 'face '(:foreground "#666666") message)
    message))

(defvar rpgdm-mythic-current-chaos-level 5 "The current, and adjustable, default chaos level")

(defun rpgdm-mythic-set-chaos-level (level)
  "Change the `rpgdm-mythic-current-chaos-level' to LEVEL.
This value can be an absolute number, or can be a relative value
if prefixed with a `+' or `-'. Notice it takes a string..."
  (interactive "sNew chaos level for current game: ")
  (let* ((parse-re (rx (optional (group (any "+" "-")))
                       (group digit)))
         (matcher  (string-match parse-re level))
         (sign     (match-string 1 level))
         (value    (string-to-number (match-string 2 level))))
    (setq rpgdm-mythic-current-chaos-level (cond
                                            ((equal sign "=") (- rpgdm-mythic-current-chaos-level value))
                                            ((equal sign "+") (+ rpgdm-mythic-current-chaos-level value))
                                            ((> value 0)      value)))))

(defun rpgdm-mythic-increase-chaos-level ()
  "Increase the current chaos level by 1."
  (interactive)
  (setq rpgdm-mythic-current-chaos-level (1+ rpgdm-mythic-current-chaos-level)))

(defun rpgdm-mythic-decrease-chaos-level ()
  "Decrease the current chaos level by 1."
  (interactive)
  (setq rpgdm-mythic-current-chaos-level (1- rpgdm-mythic-current-chaos-level)))

(defun rpgdm-mythic-random-event ()
  "Use `rpgdm-tables-choose' to select items from tables designed for Mythic.
Make sure the following tables have been loaded before calling this function:
  • `mythic/event-focus'
  • `mythic/actions'
  • `mythic/subject'
Perhaps we should make sure that we load this beforehand."
  (interactive)
  (let ((focus (rpgdm-tables-choose "mythic/event-focus"))
        (action (rpgdm-tables-choose "mythic/actions"))
        (subject (rpgdm-tables-choose "mythic/subject")))
    (rpgdm-message "Mythic Random Event- Focus: %s  Action: %s  Subject: %s"
                   (propertize focus 'face '(:foreground "yellow"))
                   (propertize action 'face '(:foreground "green"))
                   (propertize subject 'face '(:foreground "green")))))

(defun rpgdm-mythic-new-scene (chaos-factor)
  "After planning the next scene, call this to see if that happens.
The scene may change based on the given CHAOS-FACTOR.
The message display may state that the scene is altered or interrupted."
  (interactive (list (rpgdm-mythic-chaos-level)))
  (let* ((roll (rpgdm--roll-die 10))
         (mess (cond
                ((and (<= roll chaos-factor) (evenp roll)) "The scene is interrupted")
                ((and (<= roll chaos-factor) (oddp roll))  "The scene is altered")
                (t                                         "The scene proceeds as planned"))))
    (rpgdm-message mess)))

(defun rpgdm-mythic-new-adventure (name)
  "Create directory structure for adventure of NAME."
  (interactive "sName this adventure: ")
  (let* ((dirname (->> "Hunting Goblins"
                       (downcase)
                       (s-replace-all '((" " . "-")))))
         (fullpath (format "~/projects/mythic-adventures/%s" dirname))
         (overview (format "%s/overview.org" fullpath)))
    (make-directory (format "%s/players" fullpath) t)
    (make-directory (format "%s/characters" fullpath) t)
    (make-directory (format "%s/scaling-boxes" fullpath) t)
    (make-directory (format "%s/resolution-charts" fullpath) t)
    (find-file overview)))

(set-file-template! (rx "/mythic-adventures/" (one-or-more any) "/players/")
  :trigger "__mythic_rpg_player" :mode 'org-mode)
(set-file-template! (rx "/mythic-adventures/" (one-or-more any) "/characters/")
  :trigger "__mythic_rpg_character" :mode 'org-mode)
(set-file-template! (rx "/mythic-adventures/" (one-or-more any) "/scaling-boxes/")
  :trigger "__mythic_rpg_scaling" :mode 'org-mode)
(set-file-template! (rx "/mythic-adventures/" (one-or-more any) "/resolution-charts/")
  :trigger "__mythic_rpg_resolution" :mode 'org-mode)
(set-file-template! (rx "/mythic-adventures/overview.org")
  :trigger "__mythic_rpg_overview" :mode 'org-mode)

(provide 'rpgdm-mythic)
;;; rpgdm.el ends here
