;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
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

;;  Personal Emacs configuration file.

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" +path-emacs-dir))

;; Profile startup time.
(require 'init-benchmarking)

;; Configure customization file.
(setq custom-file (expand-file-name "custom.el" +path-emacs-dir))

(setq-default user-full-name "Chris Montgomery"
              user-mail-address "chris@cdom.io")

;; DEPRECATED: all of these except for the mac predicate are obvious or simple
;;             enough on their own.
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
(require 'init-frame)
(require 'init-ui-theme)
;;(require 'init-ui-nano-theme)
(require 'init-ui-modus-themes)
(require 'init-ui-font)
(require 'init-ui-icons)
(require 'init-ui-modeline)
;;(require 'init-ui-modeline-nano)
(require 'init-ui-modeline-doom)
(require 'init-ui-treemacs)


(require 'init-keys-meow)
(require 'init-hydras)
(require 'init-keys)
;; (require 'init-evil) ; TODO: comment when meow

(require 'init-clippy)
(require 'init-window)

(require 'init-selection-vertico)
(require 'init-selection-orderless)
(require 'init-selection-marginalia)
(require 'init-selection-consult)
(require 'init-completion)
(require 'init-embark)
(require 'init-wgrep)

(require 'init-editor)
(require 'init-buffers)
(require 'init-files)
(require 'init-dired)
(require 'init-vcs)
(require 'init-templates)
(require 'init-projects)

;;; memex
(require 'init-org)
(require 'init-notes)
(require 'init-notes-denote)

;;; languages
(require 'init-lsp)
(require 'init-lang-elisp)
(require 'init-lang-html)
(require 'init-lang-nix)
(require 'init-lang-php)
(require 'init-lang-yaml)
(require 'init-lang-misc)

;;; shells
(require 'init-shell-nu)

;;; tools
(require 'init-tools)
(require 'init-ledger)

;;; entertainment
(require 'init-fun)

(defun +maybe-start-server ()
  "Auto-start Emacs daemon if not already running."
  (require 'server)
  (unless (and (fboundp 'server-running-p)
               (server-running-p))
    (server-start)))
(add-hook 'elpaca-after-init-hook #'+maybe-start-server)

;; unfortunately
(when (and +graphical-p +sys-mac-p)
  (add-hook 'elpaca-after-init-hook
            (lambda () (async-shell-command "yabai --restart-service"))))

;; Load custom file after all packages have loaded.
(when (file-exists-p custom-file)
  (add-hook 'elpaca-after-init-hook
            (lambda () (load custom-file 'noerror))))

;; Wait for all packages to initialize in non-interactive mode.
(when noninteractive
  (elpaca-wait))

(provide 'init)
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; init.el ends here
