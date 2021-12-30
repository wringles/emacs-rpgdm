(defun rpgdm--fate-die ()
  "Return a cons of a Fate die and its visual representation."
  (seq-random-elt '((-1 . "[-]")
                    ( 0 . "[ ]")
                    ( 1 . "[+]"))))

(defun rpgdm--fate-roll-dice (&optional number)
  "Return a list of Fate roll results. Each element of the list
is a cons of its value and its visual representation, see
`rpgdm--fate-die'."
  punless number
    (setq number 4))
  (let (results)
    (--dotimes number
      (push (rpgdm--fate-die) results))
    results))

(defun rpgdm--fate-roll (modifier &optional number)
  "Simulate a FATE dice roll and return the results.
Return a list where the first element is the total results,
and the second element is a user-formatted view of the results. "
  (unless number (setq number 4))
  (let* ((roll (rpgdm--fate-roll-dice number))
         (vals (-map 'car roll))
         (dice (-map 'cdr roll))
         (roll (s-join " " dice))
         (sum  (apply '+ vals))
         (total (+ sum modifier))
         (results (propertize (number-to-string total) 'face '(:foreground "green"))))
    (list total (format "Rolled: %s :: %s + %d" results roll modifier))))

(defun rpgdm-fate-roll (modifier)
  "Simulate a FATE dice roll and display the results.
The total is the difference of the `+' to `-' plus the MODIFIER.
Note that the NUMBER of dice defaults to 4."
  (interactive (list (rpgdm-fate-ladder "effort level")))
  (rpgdm-message (second (rpgdm--fate-roll modifier 4))))

(let ((rpgdm-fate-ladder-values '((-2 "t" "Terrible" "Trivial") (-1 "p" "Poor" "Easy") (0 "m" "Mediocre" "Simple") (1 "a" "Average" "Average") (2 "f" "Fair" "") (3 "g" "Good" "Hard") (4 "r" "Great" "Daunting") (5 "s" "Superb" "Extreme") (6 "f" "Fantastic" "Impossible") (7 "e" "Epic" "") (8 "l" "Legendary" ""))))
(defvar rpgdm-fate-ladder rpgdm-fate-ladder-values "The FATE RPG ladder of challenge levels.")
)

(defun rpgdm-fate-ladder (&optional request-type)
  "Prompt for a choice on the FATE ladder, and return the numeric value of that level.
The REQUEST-TYPE is an optional string inserted into the prompt to describe the request."
  (interactive)
  (unless request-type (setq request-type "challenge level"))
  (let* ((choices (mapconcat 'rpgdm--fate-ladder-prompt rpgdm-fate-ladder "  "))
         (prompt  (format "What is the %s?\n%s"
                          (propertize request-type 'face '(:foreground "yellow"))
                          choices))
         (choice  (char-to-string (read-char prompt)))
         (entry   (--filter (equal (second it) choice) rpgdm-fate-ladder)))
    (first (first entry))))

(defun rpgdm--fate-ladder-prompt (entry)
  (let* ((entry-number  (format "[%d]" (first entry)))
         (render-number (propertize entry-number
                                    'face '(:foreground "#888888")))
         (keyed-prompt  (propertize (second entry) 'face '(:foreground "green"))))
    (format "%s) %s %s" keyed-prompt (third entry) render-number))))

(defun rpgdm-fate-challenge (opposition-level effort-level)
  "Return a user message for a FATE dice challenge.
Given a numeric EFFORT-LEVEL as well as the OPPOSITION-LEVEL,
this function rolls the Fate dice and interprets the results."
  (interactive (list (rpgdm-fate-ladder "opposition level")
                     (rpgdm-fate-ladder "effort level")))
  (let* ((die-roll (rpgdm--fate-roll effort-level))
         (shifts   (- (first die-roll) opposition-level))
         (results  (cond
                    ((< shifts 0) (propertize  "Failed"  'face '(:foregound "red")))
                    ((= shifts 0) (propertize  "Tie"     'face '(:foreground "yellow")))
                    ((> shifts 3) (propertize  "Succeed with Style!" 'face '(:foregound "green")))
                    (t            (propertize  "Success" 'face '(:foregound "green"))))))
    (rpgdm-message "%s ... %s" results (second die-roll))))
