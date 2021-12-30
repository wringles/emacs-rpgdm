(require 'rpgdm)

(defun rpgdm-ironsworn--results (action modifier one-challenge two-challenge)
  (let* ((action-results (+ action modifier))
         (str-results (cond
                       ((and (> action-results one-challenge) (> action-results two-challenge))
                        (propertize "Strong hit" 'face '(:foreground "green")))
                       ((or  (> action-results one-challenge) (> action-results two-challenge))
                        (propertize "Weak hit" 'face '(:foreground "yellow")))
                       (t (propertize "Miss" 'face '(:foreground "red")))))
         (matched-msg  (if (= one-challenge two-challenge)
                           (propertize " ← Create a Twist" 'face '(:foreground "orange"))
                         "")))
    (format "%s %s %d %s %d %s %d %s %d %s" str-results
            (propertize "::" 'face '(:foreground "#888"))
            action
            (propertize "+" 'face '(:foreground "#888"))
            modifier
            (propertize "→" 'face '(:foreground "#888"))
            one-challenge
            (propertize "/" 'face '(:foreground "#888"))
            two-challenge matched-msg)))

(defun rpgdm-ironsworn-roll (modifier)
  "Display a Hit/Miss message based on comparing a d6 action
roll (added to MODIFIER) vs. two d10 challenge dice."
  (interactive "nModifier: ")
  (let ((one-challenge (rpgdm--roll-die 10))
        (two-challenge (rpgdm--roll-die 10))
        (action-roll   (rpgdm--roll-die 6)))
    (rpgdm-message (rpgdm-ironsworn--results action-roll modifier
                                             one-challenge two-challenge))))

(defun rpgdm-ironsworn-progress-roll (progress-value)
  "Display a Hit/Miss message based on comparing the PROGRESS-VALUE
to rolling two d10 challenge dice."
  (interactive "nCurrent Progress Value: ")
  (let ((one-challenge (rpgdm--roll-die 10))
        (two-challenge (rpgdm--roll-die 10)))
    (rpgdm-message (rpgdm-ironsworn--results progress-value 0
                                             one-challenge two-challenge))))

(define-hash-table-test 'str-or-keys
  (lambda (a b)
    (string-equal
     (downcase
      (if (symbolp a) (symbol-name a) a))
     (downcase
      (if (symbolp b) (symbol-name b) b))))

  (lambda (s) (sxhash-equal (downcase
     (if (symbolp s) (symbol-name s) s)))))

(defvar rpgdm-ironsworn-character (make-hash-table :test 'str-or-keys)
  "Stats and attributes for the currently loaded character")

(cl-defun rpgdm-ironsworn-character (&key edge heart iron shadow wits
                                          (health 5) (spirit 5) (supply 5)
                                          (momentum 2))
  "Store the player character's stats, as well as set up the defaults for the others."
  (clrhash rpgdm-ironsworn-character)
  ;; (setq rpgdm-ironsworn-character (make-hash-table :test 'str-or-keys))
  (puthash 'edge edge rpgdm-ironsworn-character)
  (puthash 'heart heart rpgdm-ironsworn-character)
  (puthash 'iron iron rpgdm-ironsworn-character)
  (puthash 'shadow shadow rpgdm-ironsworn-character)
  (puthash 'wits wits rpgdm-ironsworn-character)

  (puthash 'health health rpgdm-ironsworn-character)
  (puthash 'spirit spirit rpgdm-ironsworn-character)
  (puthash 'supply supply rpgdm-ironsworn-character)
  (puthash 'momentum momentum rpgdm-ironsworn-character))

(defun rpgdm-ironsworn-adjust-health (health-adj)
  "Increase or decrease the current character's health by HEALTH-ADJ."
  (interactive "nHealth Adjustment: ")
  (puthash 'health
           (+ (gethash 'health rpgdm-ironsworn-character 5) health-adj)
           rpgdm-ironsworn-character))

(defun rpgdm-ironsworn-adjust-spirit (spirit-adj)
  "Increase or decrease the current character's spirit by SPIRIT-ADJ."
  (interactive "nSpirit Adjustment: ")
  (puthash 'spirit
           (+ (gethash 'spirit rpgdm-ironsworn-character 5) spirit-adj)
           rpgdm-ironsworn-character))

(defun rpgdm-ironsworn-adjust-supply (supply-adj)
  "Increase or decrease the current character's supply by SUPPLY-ADJ."
  (interactive "nSupply Adjustment: ")
  (puthash 'supply
           (+ (gethash 'supply rpgdm-ironsworn-character 5) supply-adj)
           rpgdm-ironsworn-character))

(defun rpgdm-ironsworn-adjust-momentum (momentum-adj)
  "Increase or decrease the current character's momentum by MOMENTUM-ADJ."
  (interactive "nMomentum Adjustment: ")
  (puthash 'momentum
           (+ (gethash 'momentum rpgdm-ironsworn-character 5) momentum-adj)
           rpgdm-ironsworn-character))

(defun rpgdm-ironsworn--display-stat (stat)
  (let* ((value (gethash stat rpgdm-ironsworn-character 5))
         (s-val (number-to-string value))
         (color (cond
                 ((< value 1) "red")
                 ((< value 3) "orange")
                 ((< value 4) "yellow")
                 (t "green"))))
    (propertize s-val 'face `(:foreground ,color))))

(defun rpgdm-ironsworn-character-display ()
  "Easily display the character's stats and other things."
  (interactive)
  (rpgdm-message "Edge: %d  Heart: %d  Iron: %d  Shadow: %d  Wits: %d
Health: %s  Spirit: %s  Supply: %s  Momentum: %d"
                 (gethash 'edge rpgdm-ironsworn-character 0)
                 (gethash 'heart rpgdm-ironsworn-character 0)
                 (gethash 'iron rpgdm-ironsworn-character 0)
                 (gethash 'shadow rpgdm-ironsworn-character 0)
                 (gethash 'wits rpgdm-ironsworn-character 0)

                 (rpgdm-ironsworn--display-stat 'health)
                 (rpgdm-ironsworn--display-stat 'spirit)
                 (rpgdm-ironsworn--display-stat 'supply)

                 (gethash 'momentum rpgdm-ironsworn-character 5)))

(defun rpgdm-ironsworn-roll-stat (stat modifier)
  "Roll an action based on a loaded character's STAT with a MODIFIER."
  (interactive (list (completing-read "Stat Modifier: " '(Edge Heart Iron Shadow Wits))
                     (read-string "Other Modifier: ")))
  (let ((all-mods (+ (gethash stat rpgdm-ironsworn-character)
                     (string-to-number modifier))))
    (rpgdm-ironsworn-roll all-mods)))

(defun rpgdm-ironsworn-roll-edge (modifier)
  "Roll an action based on a loaded character's Edge stat with a MODIFIER."
  (interactive (list (read-string "Edge + Modifier: ")))
  (rpgdm-ironsworn-roll-stat 'edge modifier))

(defun rpgdm-ironsworn-roll-heart (modifier)
  "Roll an action based on a loaded character's Heart stat with a MODIFIER."
  (interactive (list (read-string "Heart + Modifier: ")))
  (rpgdm-ironsworn-roll-stat 'heart modifier))

(defun rpgdm-ironsworn-roll-iron (modifier)
  "Roll an action based on a loaded character's Iron stat with a MODIFIER."
  (interactive (list (read-string "Iron + Modifier: ")))
  (rpgdm-ironsworn-roll-stat 'iron modifier))

(defun rpgdm-ironsworn-roll-shadow (modifier)
  "Roll an action based on a loaded character's Shadow stat with a MODIFIER."
  (interactive (list (read-string "Shadow + Modifier: ")))
  (rpgdm-ironsworn-roll-stat 'shadow modifier))

(defun rpgdm-ironsworn-roll-wits (modifier)
  "Roll an action based on a loaded character's Wits stat with a MODIFIER."
  (interactive (list (read-string "Wits + Modifier: ")))
  (rpgdm-ironsworn-roll-stat 'wits modifier))

(defun rpgdm-ironsworn-oracle-action-theme ()
  "Rolls on two tables at one time."
  (interactive)
  (let ((action (rpgdm-tables-choose "actions"))
        (theme  (rpgdm-tables-choose "themes")))
    (rpgdm-message "%s / %s" action theme)))

(defun rpgdm-ironsworn-oracle-npc ()
  (interactive)
  (let ((name        (rpgdm-tables-choose "names-ironlander"))
        (goal        (rpgdm-tables-choose "character-goal"))
        (role        (rpgdm-tables-choose "character-role"))
        (activity    (rpgdm-tables-choose "character-activity"))
        (description (rpgdm-tables-choose "character-descriptor"))
        (disposition (rpgdm-tables-choose "character-disposition")))
    (rpgdm-message "%s, %s %s (Activity: %s  Disposition: %s  Goal: %s)"
                   name description role activity disposition goal)))

(defun rpgdm-ironsworn-oracle-combat ()
  (interactive)
  (let ((action (rpgdm-tables-choose "combat-action"))
        (method (rpgdm-tables-choose "combat-event-method"))
        (target (rpgdm-tables-choose "combat-event-target")))
    (rpgdm-message "%s %s or %s" method target action)))

(defun rpgdm-ironsworn-oracle-feature ()
  "Rolls on two tables at one time for a Site's feature."
  (interactive)
  (let ((aspect (rpgdm-tables-choose "feature-aspect"))
        (focus  (rpgdm-tables-choose "feature-focus")))
    (rpgdm-message "%s / %s" aspect focus)))

(defun rpgdm-ironsworn-oracle-site-nature ()
  "Rolls on two tables at one time for a random Site."
  (interactive)
  (let* ((theme  (rpgdm-tables-choose "site-theme"))
         (domain (rpgdm-tables-choose "site-domain"))
         (place  (downcase domain))
         (name   (rpgdm-ironsworn-oracle-site-name place)))
    (rpgdm-message "%s %s :: %s" theme domain name)))

(defun rpgdm-ironsworn-oracle-site-name (&optional place-type)
  "Rolling on multiple tables to return a random site name."
  (interactive (list (completing-read "Place type: "
                                      '(barrow cavern icereach mine pass ruin
                                               sea-cave shadowfen stronghold
                                               tanglewood underkeep))))
  (unless place-type
    (setq place-type "unknown"))
  (let ((description (rpgdm-tables-choose "site-name-description"))
        (detail (rpgdm-tables-choose "site-name-detail"))
        (namesake (rpgdm-tables-choose "site-name-namesake"))
        (place  (rpgdm-tables-choose (format "site-name-place-%s" place-type)))
        (roll   (rpgdm--roll-die 100)))
    (rpgdm-message
     (cond
      ((<= roll 25) (format "%s %s" description place))
      ((<= roll 50) (format "%s of %s" place detail))
      ((<= roll 70) (format "%s of %s %s" place description detail))
      ((<= roll 80) (format "%s of %s's %s" place namesake detail))
      ((<= roll 85) (format "%s's %s" namesake place))
      ((<= roll 95) (format "%s %s of %s" description place namesake))
      (t            (format "%s of %s" place namesake))))))

(defvar rpgdm-ironsworn-oracle-threats '("Burgeoning Conflict" "Ravaging Horde"
                                         "Cursed Site" "Malignant Plague"
                                         "Scheming Leader" "Zealous Cult"
                                         "Environmental Calamity" "Power-Hungry Mystic"
                                         "Rampaging Creature")
  "A list of threats that correspond to tables")

(defun rpgdm-ironsworn-oracle-threat-goal (&optional category)
  "Given a CATEGORY, display a threat goal."
  (interactive (list (completing-read "Threat: " rpgdm-ironsworn-oracle-threats)))
  (unless category
    (setq category (seq-random-elt rpgdm-ironsworn-oracle-threats)))
  (let ((table-name (format "threat-%s" (downcase (string-replace " " "-" category)))))
    (rpgdm-message "%s: %s" category (rpgdm-tables-choose table-name))))

(rpgdm-ironsworn-oracle-threat-goal)

(defun rpgdm-ironsworn-oracle ()
  "Given a LIKLIHOOD as a single character, return weighted coin flip."
  (interactive)
  (let* ((prompt "What are the odds?
  c) Almost Certain  l) Likely  h) 50/50  u) Unlikely  n) Small Chance ")
         (odds (read-char prompt))
         (roll (rpgdm--roll-die 100))
         (yes! (when (or (and (= roll 11) (eq odds ?c))
                         (and (= roll 26) (eq odds ?l))
                         (and (= roll 51) (eq odds ?h))
                         (and (= roll 76) (eq odds ?u))
                         (and (= roll 91) (eq odds ?n)))
                 t))
         (yes  (when (or (and (> roll 11) (eq odds ?c))
                         (and (> roll 26) (eq odds ?l))
                         (and (> roll 51) (eq odds ?h))
                         (and (> roll 76) (eq odds ?u))
                         (and (> roll 91) (eq odds ?n)))
                 t)))
    (rpgdm-message "%s %s %s"
                   (if yes! "Extreme" "")
                   (if yes "Yes" "No")
                   (if yes! "or a twist." ""))))

(defhydra hydra-rpgdm (:color blue :hint nil)
  "
    ^Dice^                              ^Adjust^      ^Oracles/Tables^                                   ^Moving^              ^Messages^
 ----------------------------------------------------------------------------------------------------------------------------------------------------
    _d_: Roll Dice  _D_: Progress Dice  _H_: Health   _z_: Yes/No Oracle _a_: Action/Theme  _n_: NPC      _o_: Links            ⌘-‿: Show Stats
    _e_: Roll Edge  _s_: Roll Shadow    _S_: Spirit   _c_: Show Oracle   _c_: Combat Action _f_: Feature  _J_/_K_: Page up/dn     ⌘-l: Last Results
    _h_: Roll Heart _w_: Roll Wits      _G_: Supply   _O_: Other Oracle  _p_: Place Name    _P_: Place    _N_/_W_: Narrow/Widen   ⌘-k: ↑ Previous
    _i_: Roll Iron  _x_: Roll Stat      _M_: Momentum _T_: Load Oracles  _t_: Threat Goal                                   ⌘-j: ↓ Next   "
  ("d" rpgdm-ironsworn-roll)    ("D" rpgdm-ironsworn-progress-roll)
  ("z" rpgdm-ironsworn-oracle)  ("O" rpgdm-oracle)

  ("a" rpgdm-ironsworn-oracle-action-theme)
  ("n" rpgdm-ironsworn-oracle-npc)
  ("c" rpgdm-ironsworn-oracle-combat)
  ("f" rpgdm-ironsworn-oracle-feature)
  ("P" rpgdm-ironsworn-oracle-site-nature)
  ("p" rpgdm-ironsworn-oracle-site-name)
  ("t" rpgdm-ironsworn-oracle-threat-goal)

  ("e" rpgdm-ironsworn-roll-edge)
  ("h" rpgdm-ironsworn-roll-heart)
  ("i" rpgdm-ironsworn-roll-iron)
  ("s" rpgdm-ironsworn-roll-shadow)
  ("w" rpgdm-ironsworn-roll-wits)
  ("x" rpgdm-ironsworn-roll-stat :color pink)

  ("H" rpgdm-ironsworn-adjust-health :color pink)
  ("S" rpgdm-ironsworn-adjust-spirit :color pink)
  ("G" rpgdm-ironsworn-adjust-supply :color pink)
  ("M" rpgdm-ironsworn-adjust-momentum :color pink)

  ("T" rpgdm-tables-load)       ("c" rpgdm-tables-choose)     ("C" rpgdm-tables-choose :color pink)
  ("o" ace-link)                ("N" org-narrow-to-subtree)   ("W" widen)
  ("K" scroll-down :color pink)             ("J" scroll-up :color pink)

  ("s-SPC" rpgdm-ironsworn-character-display)
  ("C-m" rpgdm-last-results :color pink)
  ("C-n" rpgdm-last-results-next :color pink)
  ("C-p" rpgdm-last-results-previous :color pink)
  ("s-l" rpgdm-last-results :color pink)
  ("s-j" rpgdm-last-results-next :color pink)
  ("s-k" rpgdm-last-results-previous :color pink)

  ("q" nil "quit") ("<f12>" nil))

(provide 'rpgdm-ironsworn)
;;; rpgdm-ironsworn.el ends here
