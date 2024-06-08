;;; init-dashboard.el --- Dashboard support  -*- lexical-binding: t;  -*-

;; Copyright (c) 2024  Chris Montgomery <chmont@proton.me>

;; Author: Chris Montgomery <chmont@proton.me>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(package! enlight
  (require 'enlight)

  (setopt initial-buffer-choice #'enlight))
;; FIXME: use theme palette
(defface enlight-yellow-bold
  '((t (:foreground "#cabf00" :bold t)))
  "Yellow bold face.")
(after! enlight
  (require 'grid)

  (defvar enlight-calendar
    (progn
      (calendar)
      ;;      (diary-mark-entries)
      (prog1 (with-current-buffer (buffer-name (current-buffer))
               (buffer-string))
        (calendar-exit))))

  (setopt enlight-content
          (concat (grid-get-box `(:align center :content "C E A M X" :width 80))
                  enlight-calendar "\n"
                  (grid-get-row
                   (list (concat
                          (propertize "MENU" 'face 'highlight) "\n"
                          (enlight-menu '(("Org-Mode"
                                           ("Agenda (current day)" (org-agenda nil "a") "A"))
                                          ("Contexts"
                                           ("Activity..." activities-resume "a")
                                           ("Project..." project-switch-project "p"))
                                          ("Open"
                                           ("~/Downloads/" (dired "~/Downloads") "d"))))))))))
;;;; TODO: not yet working, might not be do-able in the early days of `enlight'

;; (keymap-set enlight-mode-map "a" (define-prefix-command '+enlight-menu-a-prefix))

;; (grid-get-column (list (propertize "[ACTIVITIES]" 'face 'highlight)
;;                        `(:content ,(enlight-menu `(("Seadome"
;;                                                     ()
;;                                                     ("CEAMX" (activities-resume (activities-named "ceamx")) "a c")
;;                                                     ("Dotfield" (activities-resume (activities-named  "dotfield") "a d")))
;;                                                   ("Kleinweb"
;;                                                    ("TUTV" (activities-resume (activities-named "tutv")) "a w 1"))))
;;                          :width 50)))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
