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

(defvar rpgdm-base ".")

(defvar rpgdm-screen-directory
  (expand-file-name "dnd-5e" rpgdm-base)
  "Directory path containing the tables to load and create functions.")

(defvar rpgdm-screen-files nil
  "Associative list of files and their titles.")

(defvar rpgdm-screen-window-side t)
(defvar rpgdm-screen-window-size nil)
(defvar rpgdm-screen-fullscreen nil)


(defun rpgdm-screen-show (file-title)
  "Display in a side-window, FILE-TITLE.
Interactively, display a list of files in `rpgdm-screen-directory'."
  (interactive (list (completing-read "Show screen: "
                                      (rpgdm-screen-screen-list))))
  (let ((filename (alist-get file-title (rpgdm-screen-screen-list) nil nil 'equal)))
    (unless rpgdm-screen-fullscreen
      (delete-other-windows))
    (save-excursion
      (select-window
       (split-window (frame-root-window)
                     rpgdm-screen-window-size
                     rpgdm-screen-window-side))
      (find-file filename))))

(defun rpgdm-quick-close ()
  "Easily close the current chart."
  (interactive)
  (kill-buffer)
  (delete-window))

(defun rpgdm-screen-screen-list ()
  "A memoized list of cons cells containing the title and fully-qualified filename."
  (unless rpgdm-screen-files
    (dolist (file (directory-files rpgdm-screen-directory t))
      (add-to-list 'rpgdm-screen-files (cons (rpgdm-screen--read-title file) file))))
  rpgdm-screen-files)

(defun rpgdm-screen--read-title (file)
  "Return the title of an org-formatted FILE or its first line of text."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward (rx bol "#+title:" (one-or-more space)
                               (group (one-or-more any))) nil t)
        (match-string 1)
      (goto-char (point-min))
      (buffer-substring-no-properties (point-min) (line-end-position)))))


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
    (rpgdm-message (s-trim item))))

(defun rpgdm-screen-choose-sublist ()
  "Randomly choose an elemeent from the lists in the subtree.
The contents of the item is displayed in the mini-buffer."
  (interactive)
  (save-excursion
    (org-narrow-to-subtree)
    (rpgdm-screen-choose-list)
    (widen)))

(defun rpgdm-screen (&optional filepath)
  "Hard-coded (currently) approach to displaying files from FILEPATH."
  (interactive (list (read-directory-name "DM Screen Directory: "
                                          rpgdm-screen-directory)))
  (delete-other-windows)

  (let ((default-directory filepath))
    ;; Start the Right Side with DIRED
    (split-window-right)
    (other-window 1)
    (dired ".")
    (dired-hide-details-mode)

    (split-window-right)
    (find-file "actions.org")

    (split-window-below)
    (split-window-below)
    (other-window 1)
    (find-file "weather-effects.org")
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
    (find-file "armor.org")
    (split-window-below)
    (other-window 1)
    (find-file "conditions.org")
    (other-window 1)))

(provide 'rpgdm-screen)
;;; rpgdm-screen.el ends here
