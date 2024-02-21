;;; init-session-start.el --- Session startup customizations  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
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

;; What happens when Emacs starts?

;; Here we configure things like `desktop-save-mode' and the dashboard.

;;; Code:

(require 'ceamx-paths)

(require 'lib-common)

;;; Save and restore Emacs sessions with `desktop-save-mode'

(use-feature! desktop
  :config
  (setopt desktop-dirname ceamx-var-dir)
  (setopt desktop-base-file-name "ceamx.desktop")
  (setopt desktop-base-lock-name (concat desktop-base-file-name ".lock"))

  ;; Save session state after five minutes idle.
  (setopt desktop-auto-save-timeout (* 60 5))

  ;; If the desktop file is still locked, that probably means something went
  ;; wrong during the previous Emacs session because each session should remove
  ;; its locks when exiting. Play it safe and do nothing.
  (setopt desktop-load-locked-desktop nil)

  ;; All other buffers will be restored lazily during idle.
  (setopt desktop-restore-eager 20)

  ;; This is kind of the whole point to me.
  (setopt desktop-restore-frames t)

  ;; Always save the session without asking and regardless of whether a previously-saved
  ;; session file exists.
  (setopt desktop-save t)

  (desktop-save-mode 1))

(provide 'init-session-start)
;;; init-session-start.el ends here
