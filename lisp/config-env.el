;;; config-env.el --- Variables regarding the environment  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

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

;; FIXME: should not be `const's! see <https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Variables.html#index-defconst>

;; Where am I?

;;; Code:

(defconst +gui-p
  (display-graphic-p))

(defconst +xorg-p
  (memq window-system '(x)))

(defconst +user-root-p
  (string-equal "root" (getenv "USER")))

(defconst +sys-mac-p
  (or (memq window-system '(mac ns))
      (eq system-type 'darwin)))

(defconst +sys-linux-p
  (eq system-type 'gnu/linux))

(defconst +env-dumb-p
  (string= (getenv "TERM") "dumb"))

(defconst +env-iterm-p
  (string= (getenv "TERM_PROGRAM") "iTerm.app"))

(defconst +env-xterm-p
  (not (string-empty-p (getenv "XTERM_VERSION"))))

(defconst +env-gnome-terminal-p
  (string= (getenv "COLORTERM") "gnome-terminal"))

(defconst +env-konsole-p
  (not (string-empty-p (getenv "KONSOLE_PROFILE_NAME"))))

(defconst +env-apple-terminal-p
  (string= (getenv "TERM_PROGRAM") "Apple_Terminal"))

(provide 'config-env)
;;; config-env.el ends here
