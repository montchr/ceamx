;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/ceamx
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

(add-to-list 'load-path (expand-file-name "lisp" +path-emacs-dir))

;; Profile startup time.
(require 'init-benchmarking)

;; Configure customization file.
(setq custom-file (expand-file-name "custom.el" +path-emacs-dir))

(setq-default user-full-name "Chris Montgomery"
              user-mail-address "chris@cdom.io")

(defconst +graphical-p (display-graphic-p))
(defconst +xorg-p (memq window-system '(x)))
(defconst +user-root-p (string-equal "root" (getenv "USER")))
(defconst +sys-mac-p (or (memq window-system '(mac ns)) (eq system-type 'darwin)))
(defconst +sys-linux-p (eq system-type 'gnu/linux))

(require 'init-packages)

(require 'cl-lib)
(require 'map)

(use-package s)
(use-package dash)

(require 'lib-common)
(require 'lib-doom)

(require 'init-defaults)
(require 'init-env)

(when +graphical-p
  (require 'lib-gui))

(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-theme)
(require 'init-frame)
(require 'init-modeline)

;;(require 'init-keys-meow)
(require 'init-hydras)
(require 'init-keybinds)
(require 'init-evil)

(require 'init-clippy)
(require 'init-window)
(require 'init-completion)
(require 'init-editor)
(require 'init-buffers)
(require 'init-files)

(require 'init-dired)
(require 'init-vcs)
(require 'init-templates)
(require 'init-projects)
(require 'init-lsp)
(require 'init-org)

;; languages
(require 'init-lang-elisp)
(require 'init-lang-nix)
(require 'init-lang-misc)

;; Auto-start Emacs daemon if not already running.
(unless (and (fboundp 'server-running-p) (server-running-p))
  (server-start))

;; Load custom file after all packages have loaded.
(when (file-exists-p custom-file)
  (add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror))))

;; Wait for all packages to initialize in non-interactive mode.
(when noninteractive
  (elpaca-wait))

(provide 'init)
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; init.el ends here
