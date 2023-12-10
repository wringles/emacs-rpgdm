;;; rpgdm-npc.el --- Random NPC Generation -*- lexical-binding: t; -*-
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
;;    Choosing a good NPC is about the location:
;;      - City
;;      - Town
;;      - Village
;;      - Road
;;      - Feywild, etc.
;;
;;    As this will really affect the occupation.
;;
;;    Also, the /purpose/ may also change whether we are looking for a companion
;;    or guild, or just a random person on the street.
;;
;;    Obviously, I won't worry about giving incorrect answers, as anything that
;;    doesn't /fit/ will just be ignored.
;;
;;;
;;; CCode:

(require 'rpgdm-core (expand-file-name "rpgdm-core.el" rpgdm-base) t)
(require 'rpgdm-dice (expand-file-name "rpgdm-dice.el" rpgdm-base) t)
(require 'rpgdm-tables (expand-file-name "rpgdm-tables.el" rpgdm-base) t)

(defun rpgdm-npc-gender-name ()
  "Return nil or non-nil for male or female names."
  (= (random 2) 0))

(defun rpgdm-npc-choose (&rest choices)
  "Randomly return from from CHOICES, one of the parameters."
  (seq-random-elt choices))

(defun rpgdm-npc-adventuring-age ()
  "Return string representing the age of an NPC available to physically aide the adventures."
  (let ((roll (rpgdm--roll-die 100)))
    (cond
     ((= roll 1)   "Very young (child)")
     ((<= roll 10) "Coming of age")
     ((<= roll 60) "Young adult")
     ((<= roll 90) "Middle Aged")
     ((<= roll 99) "Old")
     (t "Past normal lifespan (very old)"))))

(defun rpgdm-npc-age ()
  "Return string representing an age of a random NPC."
  (let ((roll (rpgdm--roll-die 100)))
    (cond
     ((= roll 1)   "Very young (child)")
     ((<= roll 10) "Coming of age")
     ((<= roll 40) "Young adult")
     ((<= roll 70) "Middle Aged")
     ((<= roll 96) "Old")
     (t "Past normal lifespan (very old)"))))

(defun rpgdm-npc-known-parents ()
  "Return non-nil if PC knew their parents."
  (< (rpgdm--roll-die 100) 96))

(defun rpgdm-npc--parents ()
  "Return a tuple of `father' and `mother' in either order."
  (seq-random-elt '((father mother) (mother father))))

(defun rpgdm-npc--parent-assignment (x y)
  "Given two parental types, X and Y, assign them to father or mother."
  (seq-let (a b) (rpgdm-npc--parents)
    (format "%s was %s, and %s was %s" a x b y)))

(defun rpgdm-npc-halfelf-parents ()
  (let ((roll (rpgdm--roll-die 8)))
    (cond
     ((<= roll 5) (rpgdm-npc--parent-assignment "an elf" "human"))
     ((= roll 6)  (rpgdm-npc--parent-assignment "an elf" "half-elven"))
     ((= roll 7)  (rpgdm-npc--parent-assignment "human" "half-elven"))
     ((= roll 8)  "both parents were half-elven"))))

(defun rpgdm-npc-halforc-parents ()
  (let ((roll (rpgdm--roll-die 8)))
    (cond
     ((<= roll 3) (rpgdm-npc--parent-assignment "an orc" "human"))
     ((= roll 5)  (rpgdm-npc--parent-assignment "an orc" "a half-orc"))
     ((= roll 7)  (rpgdm-npc--parent-assignment "human" "a half-orc"))
     ((= roll 8)  "both parents were half-orcs"))))

(defun rpgdm-npc-tiefling-parents ()
  (let ((roll (rpgdm--roll-die 8)))
    (cond
     ((<= roll 4) "both parents were human, their infernal heritage dormant until their child was born")
     ((= roll 6)  (rpgdm-npc--parent-assignment "a tiefling" "human"))
     ((= roll 7)  (rpgdm-npc--parent-assignment "a tiefling" "a devil"))
     ((= roll 8)  (rpgdm-npc--parent-assignment "human" "a devil")))))

(defun rpgdm-npc-birthplace ()
  "Return string of a random birthplace."
  (let ((roll (rpgdm--roll-die 100)))
    (cond
     ((<= roll 50) "at home")
     ((<= roll 55) "in the home of a family friend")
     ((<= roll 63) "in the home of a healer or midwife")
     ((<= roll 65) (format "in a %s" (rpgdm-npc-choose "carriage" "cart or wagon")))
     ((<= roll 68) "in a barn, shed, or other outbuilding")
     ((<= roll 70) "in a cave")
     ((<= roll 72) "in a field")
     ((<= roll 74) "in a forest")
     ((<= roll 77) "in a Temple")
     ((= roll 78)  "in a battlefield")
     ((<= roll 80) "in an alley or street")
     ((<= roll 82) "in a brothel, tavern, or inn")
     ((<= roll 84) "in a castle, keep, tower, or palace")
     ((= roll 85)  (format "in %s" (rpgdm-npc-choose "the sewers" "a rubbish heap")))
     ((<= roll 88) (format "among people of a different race: %s" (rpgdm-tables-choose "npc-race")))
     ((<= roll 91) (format "on board a %s" (rpgdm-npc-choose "boat" "ship")))
     ((<= roll 93) (format "in %s of a secret organization" (rpgdm-npc-choose "a prison" "the headquarters")))
     ((<= roll 95) "in a sage’s laboratory")
     ((= roll 96)  "in the Feywild")
     ((= roll 97)  "in the Shadowfell")
     ((= roll 98)  (format "on the %s Plane" (rpgdm-npc-choose "Astral" "Ethereal")))
     ((= roll 99)  "on an Inner Plane")
     (t            "on an Outer Plane"))))

(defun rpgdm-npc--cause-of-death ()
  "Return string from a random cause of death."
  (let ((roll (rpgdm--roll-die 12)))
    (cond
     ((= roll 1) "unknown")
     ((= roll 2) "murdered")
     ((= roll 3) "killed in battle")
     ((= roll 4) (format "accident related to %s"
                         (rpgdm-npc-choose "class" "occupation")))
     ((= roll 5) "accident unrelated to class or occupation")
     ((= roll 6) "disease, natural causes")
     ((= roll 7) "old age, natural causes")
     ((= roll 8) "apparent suicide")
     ((= roll 9) (rpgdm-npc-choose "torn apart by an animal" "natural disaster"))
     ((= roll 10) "consumed by a monster")
     ((= roll 11) (rpgdm-npc-choose "executed for a crime" "tortured to death"))
     ((= roll 12) "bizarre event, such as being hit by a meteorite, struck down by an angry god, or killed by a hatching slaad egg"))))

(defun rpgdm-npc-siblings ()
  "Return string of the number of siblihngs.
I am not sure why Xanathar's Guide had such a complicated
way of rolloing for siblings. But I recreated it."
  (let ((roll (rpgdm--roll-die 10)))
    (cond
     ((<= roll 2) 0)
     ((<= roll 4) (rpgdm-roll-sum "1d3"))
     ((<= roll 6) (rpgdm-roll-sum "1d4+1"))
     ((<= roll 8) (rpgdm-roll-sum "1d6+2"))
     ((<= roll 10) (rpgdm-roll-sum "1d8+3")))))

(defun rpgdm-npc-birth-order ()
  "A string results of whether a sibling is older or younger."
  (let ((roll (rpgdm-roll-sum "2d6")))
    (cond
     ((= roll 2)  "Twin, triplet, or quadruplet")
     ((<= roll 7) "Older")
     (t           "Younger"))))

(provide 'rpgdm-npc)
;;; rpgdm-npc.el ends here
