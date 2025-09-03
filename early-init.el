;;; early-init.el --- Early initialization file  -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (c) 2022-2025  Chris Montgomery <chmont@protonmail.com>

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
;;; Code:

(setq package-enable-at-startup nil)

(setq garbage-collection-messages t)

(dolist (subdir '("autoloads" "lisp" "site-lisp"))
  (let ((dir (expand-file-name subdir user-emacs-directory)))
    (add-to-list 'load-path dir)))

(require 'ceamx-paths)
(require 'ceamx-lib)

(setq package-user-dir ceamx-packages-dir)

(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "eln-cache/" ceamx-packages-dir))))

(setq native-comp-async-report-warnings-errors 'silent)
(setq native-compile-prune-cache t)

(setq load-prefer-newer t)

(setq byte-compile-warnings nil)

(setq inhibit-x-resources t)

(defconst ceamx-ui-gsettings-ui-namespace "org.gnome.desktop.interface")

(defvar ceamx-ui-tiling-window-manager-regexp "sway"
  "Regular expression matching supported tiling window managers.")

(defmacro with-desktop-session! (&rest body)
  "Expand BODY if desktop session is not a tiling window manager.
See `ceamx-ui-tiling-window-manager-regexp' for the definition of
supported tiling window managers."
  (declare (indent 0))
  `(when-let* ((session (getenv "DESKTOP_SESSION"))
               (not (string-match-p session ceamx-ui-tiling-window-manager-regexp)))
     ,@body))

(defun ceamx-ui-gsettings-theme ()
  "Get the currently-active GNOME/GTK color scheme."
  (shell-command-to-string (format "gsettings get %s color-scheme"
                                   ceamx-ui-gsettings-ui-namespace)))

(defun ceamx-ui-gsettings-dark-theme-p ()
  "Whether GNOME/GTK are using a theme with a dark color scheme."
  (string-match-p "dark" (ceamx-ui-gsettings-theme)))

(defun ceamx-ui-desktop-dark-theme-p ()
  "Predicate whether a desktop environment is displaying a dark appearance."
  (or (ceamx-ui-gsettings-dark-theme-p)))

(defun ceamx-ui-re-enable-frame-theme (_frame)
  "Re-enable active theme, if any, upon FRAME creation.
Add this to `after-make-frame-functions' so that new frames do
not retain the generic background set by the function
`ceamx-init-prevent-initial-light-flash'."
  (when-let* ((theme (car custom-enabled-themes)))
    (enable-theme theme)))

(setq frame-resize-pixelwise t
  frame-inhibit-implied-resize t
  frame-title-format '("%b")
  use-dialog-box t
  use-file-dialog nil
  use-short-answers t              ; affects `yes-or-no-p'
  read-answer-short t              ; affects `read-answer' (completion)
  inhibit-splash-screen t
  inhibit-startup-screen t
  inhibit-startup-echo-area-message user-login-name
  inhibit-startup-buffer-menu t)

(provide 'early-init)
;;; early-init.el ends here
