;;; lib-tools.el --- Library for miscellaneous tooling  -*- lexical-binding: t;  -*-

;; Copyright (c) 2024  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>
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

(defun ceamx-finance/hledger-next-entry ()
  "Move to next entry and pulse."
  (interactive)
  (declare-function hledger-next-or-new-entry "hledger-mode")
  (declare-function hledger-pulse-momentary-current-entry "hledger-mode")

  (hledger-next-or-new-entry)
  (hledger-pulse-momentary-current-entry))

(defun ceamx-finance/hledger-prev-entry ()
  "Move to last entry and pulse."
  (interactive)
  (declare-function hledger-backward-entry "hledger-mode")
  (declare-function hledger-pulse-momentary-current-entry "hledger-mode")

  (hledger-backward-entry)
  (hledger-pulse-momentary-current-entry))
(defun +hledger-accounts-completion-at-point ()
  "Return completion candidates for hledger accounts."

  (when-let ((bounds (and (boundp 'hledger-accounts-cache)
                          (bounds-of-thing-at-point 'symbol))))
    (list (car bounds) (point) hledger-accounts-cache)))

(defun +hledger-accounts-capf-h ()
  "Add hledger accounts to `completion-at-point' functions."
  (add-hook 'completion-at-point-functions
            '+hledger-accounts-completion-at-point 20 t))

(provide 'lib-tools)
;;; lib-tools.el ends here
