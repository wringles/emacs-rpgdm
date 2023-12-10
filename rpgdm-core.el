;;; rpgdm-core --- Core functionality for rpgdm -*- lexical-binding: t -*-
;;
;; Copyright (C) 2021 Howard X. Abrams
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;;         Jeremy Friesen <jeremy@jeremyfriesen.com>
;; Maintainer: Howard X. Abrams
;; Created: December 10, 2023
;;
;; This file is NOT part of GNU Emacs.
;;; Commentary:
;;
;;    There are functions shared across different `rpgdm' packages.  These are
;;    considered "core" functionality.
;;
;;; Code:
(defvar rpgdm-base
  (seq-find (lambda (elt) (string-match "rpgdm" elt)) load-path (getenv "HOME"))
  "Default directory to look for supporting data, like tables and charts.")

(defvar rpgdm-last-results (make-ring 10)
  "The results from calls to `rpgdm-screen-' functions are stored here.")

(defvar rpgdm-last-results-ptr 0
  "Keeps track of where we are in the message display ring.
Each call to `rpgdm-last-results' resets this to 0.")

(defun rpgdm-message (format-string &rest args)
  "Replace `message' function allowing it to be re-displayed.
The FORMAT-STRING is a standard string for the `format' function,
and ARGS are substitued values."
  (let ((message (apply 'format format-string args)))
    (ring-insert rpgdm-last-results message)
    (kill-new message)
    (rpgdm-last-results)))

(defun rpgdm-last-results ()
  "Display results from the last call to a `rpgdm-message' function."
  (interactive)
  (setq rpgdm-last-results-ptr 0)
  (message (ring-ref rpgdm-last-results rpgdm-last-results-ptr)))

(defun rpgdm-last-results-previous ()
  "Display results from an earlier call to `rpgdm-message'."
  (interactive)
  (cl-incf rpgdm-last-results-ptr)
  (when (>= rpgdm-last-results-ptr (ring-length rpgdm-last-results))
    (setq rpgdm-last-results-ptr 0))
  (message "%d> %s" rpgdm-last-results-ptr (ring-ref rpgdm-last-results rpgdm-last-results-ptr)))

(defun rpgdm-last-results-next ()
  "Display results from an later call to `rpgdm-message'.
Meant to be used with `rpgdm-last-results-previous'."
  (interactive)
  (when (> rpgdm-last-results-ptr 0)
    (cl-decf rpgdm-last-results-ptr))
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

(provide 'rpgdm-core)
;;; rpgdm-core.el ends here
