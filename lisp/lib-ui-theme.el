;;; lib-ui-theme.el --- Theme library functions and macros  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery
;; Copyright (C) 2024  Protesilaos Stavrou

;; Author: Chris Montgomery <chris@cdom.io>
;;         Protesilaos Stavrou <info@protesilaos.com>
;; Keywords: local

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

;; TODO: revisit this approach

;;; Code:

(defun ceamx-reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes `(,ceamx-ui-theme-light))
  (ceamx-reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes `(,ceamx-ui-theme-dark))
  (ceamx-reapply-themes))

;; via prot-emacs
(defun ceamx-theme-re-enable-in-frame (_frame)
  "Re-enable active theme, if any, upon FRAME creation.
Add this to `after-make-frame-functions' so that new frames do
not retain the generic background set by the function
`ceamx-theme-no-bright-flash'."
  (when-let ((theme (car custom-enabled-themes)))
    (enable-theme theme)))



(provide 'lib-ui-theme)
;;; lib-ui-theme.el ends here
