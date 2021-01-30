;;; rpgdm-screen.el --- Support for viewing/creating a Dungeon Master's Screen -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Howard X. Abrams
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams <howard.abrams@gmail.com>
;; Created: December 15, 2020
;;
;;; Commentary:
;;
;; A collection of functions to help make a Dungeon Master Screen in Emacs.
;; This assumes functions are called in an org-formatted file.
;;
;; For instance, let's suppose most of the file is a list, and you want to
;; choose one, you can have:
;;
;;         [[elisp:(rpgdm-screen-choose-list)][Choose one]]:
;;
;;; Code:

(require 'org)
(require 'org-element)
(require 's)

(defvar rpgdm-screen-last-results ""
  "The results from calls to `rpgdm-screen-' functions are stored here.")

(defun rpgdm-screen-last-results ()
  "Display results from the last call to a `rpgdm-screen-' function."
  (interactive)
  (message rpgdm-screen-last-results))

(defun rpgdm-screen--get-list-items ()
  "Return a list of all the list items in the org document."
  (org-element-map (org-element-parse-buffer) 'item
    (lambda (item)
      (buffer-substring-no-properties
          (org-element-property :contents-begin item)
          (org-element-property :contents-end item)))))

(defun rpgdm-screen-choose-list ()
  "Randomly choose an elemeent from all lists in the current file.
The contents of the item is displayed in the mini-buffer."
  (interactive)
  (let* ((items (rpgdm-screen--get-list-items))
         (item (nth (random (length items)) items)))
    (setq rpgdm-screen-last-results (s-trim item))
    (message rpgdm-screen-last-results)))

(defun rpgdm-screen-choose-sublist ()
  "Randomly choose an elemeent from the lists in the subtree.
The contents of the item is displayed in the mini-buffer."
  (interactive)
  (save-excursion
    (org-narrow-to-subtree)
    (rpgdm-screen-choose-list)
    (widen)))

(defun rpgdm-screen ()
  (interactive)
  (delete-other-windows)

  ;; Start the Right Side with DIRED
  (split-window-right)
  (other-window 1)
  (dired "dnd-5e")
  (dired-hide-details-mode)

  (split-window-right)
  (find-file "skill-checks.org")

  (split-window-below)
  (split-window-below)
  (other-window 1)
  (find-file "names.org")
  (other-window 1)
  (find-file "magic-schools.org")
  (split-window-below)
  (other-window 1)
  (find-file "costs.org")
  (other-window 1)

  ;; On the far right window:
  (split-window-below)
  (split-window-below)
  (other-window 1)
  (find-file "gear.org")
  (other-window 1)
  (find-file "trinkets.org")
  (split-window-below)
  (other-window 1)
  (find-file "conditions.org")
  (other-window 1)
  )

(provide 'rpgdm-screen)
;;; rpgdm-screen.el ends here
