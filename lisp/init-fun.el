;;; init-fun.el --- Configure the fun                -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; It's fun.

;;; Code:

(require 'ceamx-lib)

;;;; ~zone~ [builtin]

;; <https://www.emacswiki.org/emacs/ZoneMode>

(use-feature! zone
  :autoload (zone-when-idle)
  :defines (zone-timer)

  :config
  ;; TODO: verify
  (def-hook! ceamx-zone-when-idle-h ()
    'ceamx-emacs-startup-hook
    "Zone out when idle.
Return the new `zone' timer."
    (zone-when-idle (* 60 10))))

;; FIXME: broken: wrong type argument arrayp (for pgm arg)
;;        (where did this even come from? emacswiki?)
;; (defun zone-choose (pgm)
;;   "Choose a PGM to run for `zone'."
;;   (interactive
;;     (list
;;       (completing-read
;;         "Program: "
;;         (mapcar 'symbol-name zone-programs))))
;;   (let ((zone-programs (list (intern pgm))))
;;     (zone)))

(provide 'init-fun)
;;; init-fun.el ends here
