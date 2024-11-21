;;; config-env.el --- Variables regarding the environment  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
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

;; Where are we?

;;; Code:

(defvar +gui-p
  (display-graphic-p))

(defvar +xorg-p
  (memq window-system '(x)))

(defvar +user-root-p
  (string-equal "root" (getenv "USER")))

(defvar +sys-mac-p
  (or (memq window-system '(mac ns))
      (eq system-type 'darwin)))

(defvar +sys-linux-p
  (eq system-type 'gnu/linux))

(defvar +env-pgtk-p
  (bound-and-true-p pgtk-initialized)
  "Whether Emacs is running with pure-GTK windowing.")

;; via <https://emacsredux.com/blog/2021/12/19/wsl-specific-emacs-configuration/>
(defvar +sys-wsl-p
  (and (eq system-type 'gnu/linux)
       (or (getenv "WSLENV")
           (getenv "WSL_DISTRO_NAME")))
  "Whether Emacs is currently running in WSL.")

(defvar +env-dumb-p
  (string= (getenv "TERM") "dumb"))

;; TODO: is this really the way? and is it even necessary?
(defvar +env-iterm-p
  (string= (getenv "TERM_PROGRAM") "iTerm.app"))

(defvar +env-xterm-p
  (not (string-empty-p (getenv "XTERM_VERSION"))))

(defvar +env-gnome-terminal-p
  (string= (getenv "COLORTERM") "gnome-terminal"))

(defvar +env-konsole-p
  (not (string-empty-p (getenv "KONSOLE_PROFILE_NAME"))))

(defvar +env-apple-terminal-p
  (string= (getenv "TERM_PROGRAM") "Apple_Terminal"))

(provide 'config-env)
;;; config-env.el ends here
