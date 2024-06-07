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

(after! enlight
  (require 'grid)

  ;; FIXME: use theme palette
  (defface enlight-yellow-bold
    '((t (:foreground "#cabf00" :bold t)))
    "Yellow bold face.")

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
                                           ("Agenda (current day)" (org-agenda nil "a") "a"))
                                          ("Projects"
                                           ("switch..." project-switch-project "p"))
                                          ("Downloads"
                                           ("~/Downloads/" (dired "~/Downloads") "a"))))))))))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
