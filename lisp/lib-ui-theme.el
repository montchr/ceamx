;;; lib-ui-theme.el --- Theme library functions and macros  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords:

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

;; Theme library functions and macros.

;; TODO: revist this approach

;; FIXME: doesn't load properly after restarting emacs (but neither does `exec-path-from-shell' -- might be the elpaca hook?)

;;; Code:

(defun cmx-reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes `(,cmx-ui-theme-light))
  (cmx-reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes `(,cmx-ui-theme-dark))
  (cmx-reapply-themes))


(provide 'lib-ui-theme)
;;; lib-ui-theme.el ends here
