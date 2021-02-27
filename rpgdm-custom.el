;;; rpgdm-custom.el --- Customizations for RPGDM -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Howard X. Abrams
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams <howard.abrams@workday.com>
;; Created: February 26, 2021
;;
;; This file is not part of GNU Emacs.
;;
;;
;;; Code:

(defgroup rpgdm nil
  "Customization for the Dungeon Master support package."
  :prefix "rpgdm-"
  :group 'applications
  :link '(url-link :tag "Github" "https://gitlab.com/howardabrams/emacs-rpgdm"))

(defcustom rpgdm-screen-window-side :right
  "Split new windows on a particular side.
For instance, `:below' or on the `:right'."
  :type '(choice (const :tag "above" 'above)
                 (const :tag "below" 'below)
                 (const :tag "left"  'left)
                 (const :tag "right" 'right))
  :group 'rpgdm)

(defcustom rpgdm-screen-window-size nil
  "The size of new windows. Leave nil for half the frame."
  :type '(integer)
  :group 'rpgdm)

(defcustom rpgdm-screen-fullscreen nil
  "Should the displayed screen occupy the entire frame?"
  :type '(boolean)
  :group 'rpgdm)


(provide 'rpgdm-custom)
;;; rpgdm-custom.el ends here
