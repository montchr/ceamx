;;; init.el --- Personal configuration file -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/dotfield
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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

;;  Personal Emacs configuration file.

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" +path-config-dir))

;; Profile startup time.
(require 'init-benchmarking)

;; Configure customization file.
(setq custom-file (expand-file-name "custom.el" +path-config-dir))

(setq-default user-full-name "Chris Montgomery"
              user-mail-address "chris@cdom.io")

(defconst +is-graphical (display-graphic-p))
(defconst +is-root-user (string-equal "root" (getenv "USER")))
(defconst +is-sys-mac (eq system-type 'darwin))
(defconst +is-sys-linux (eq system-type 'gnu/linux))
(defconst +env-sys-name (system-name))

;; Tame fullscreen behavior across window managers.
(pcase window-system
  ('w32 (set-frame-parameter nil 'fullscreen 'fullboth))
  (_ (set-frame-parameter nil 'fullscreen 'maximized)))

(require 'init-defaults)
(require 'init-packages)

(require 'init-theme)
(require 'init-keys)
(require 'init-editor)
(require 'init-vcs)

;; TODO: reload yabai on init
;; unfortunately
;; (when (and
;;        (not elpa-bootstrap-p)
;;        env-graphic-p
;;        env-sys-mac-p)
;;   (add-hook 'after-init-hook
;;             (lambda ()
;;               (shell-command-to-string "yabai-relaunch"))))

;; Load custom file.
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(provide 'init)
;;; init.el ends here
