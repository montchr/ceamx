;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/ceamx

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

;;; Configure module load path.
(add-to-list 'load-path (expand-file-name "lisp" +path-emacs-dir))
(add-to-list 'load-path (expand-file-name "lisp/lib" +path-emacs-dir))

;;; Profile startup time.
(require 'init-benchmarking)

;;; Configure customization file.
(setq custom-file (expand-file-name "custom.el" +path-emacs-dir))

;;; Define default user identity.
(setq-default user-full-name "Chris Montgomery"
              user-mail-address "chris@cdom.io")

;;; Load environment-related constants.
(require 'config-env)

;;; Initialize package loading.
(require 'init-packages)


;;
;;; Libraries
;;

;;; Bundled with Emacs
(require 'cl-lib)
(require 'map)

;;; Install common library packages
(use-package s)     ; strings => <https://github.com/magnars/s.el>
(use-package dash)  ; lists => <https://github.com/magnars/dash.el>
(use-package f)     ; files => <https://github.com/rejeep/f.el>
(use-package ht)    ; hash tables => <https://github.com/Wilfred/ht.el>
(use-package llama) ;  `##' lambda shorthand => <https://git.sr.ht/~tarsius/llama>

(require 'lib-common)

(when (display-graphic-p)
  (require 'lib-gui))


(require 'lib-files)

;; Prepend the site-lisp directory and its immediate subdirectories to the load path.
(push +path-site-lisp-dir load-path)
(cmx-prepend-subdirs-to-load-path +path-site-lisp-dir)

;;; site-lisp packages
(require 'on)


;;
;;; Configuration
;;

;; Generally sorted in order of likelihood of first user interaction.
;;
;; An interaction can be one-way or two-way:
;; keep in mind that perception is still a (one-way) interaction,
;; so legibility of displayed information is important immediately,
;; even if the interface cannot respond to input.

(require 'init-defaults)

(require 'init-env)

;;; Displays + Appearance

(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-frame)

;;;; Theme
(require 'init-ui)
(require 'init-ui-theme)
;; TODO: probably not for tty
(require 'init-ui-modus-themes)
;; (require 'init-ui-nano-theme)

;;;; Typography + Iconography
(when (display-graphic-p)
  (require 'init-ui-font))
(require 'init-ui-icons)

;;;; Modeline
(require 'init-ui-modeline)
;;(require 'init-ui-modeline-nano)
;; TODO: maybe maybe not?
(when (display-graphic-p)
  (require 'init-ui-modeline-doom))

;;;; Sidebar
;; TODO: figure out how to load as late as possible?
(require 'init-ui-treemacs)

;;; Keybindings
(require 'init-keys)
(require 'init-keys-evil)
(require 'init-keys-bindings)

;;; Window
(require 'init-window)
(require 'init-buffer)
(require 'init-history)

;; TODO: split up clippy (it's vague)
;; TODO: why here? explain
(require 'init-clippy)

;;; Selection

(require 'init-selection-vertico)
(require 'init-selection-orderless)
(require 'init-selection-marginalia)
(require 'init-selection-consult)
(require 'init-completion)

;;; Actions

(require 'init-embark)
(require 'init-wgrep)

;;; Editing, buffers, files

(require 'init-editor)
(require 'init-files)
(require 'init-dired)

;;; Project + Workspace

(require 'init-vcs)
(require 'init-templates)
(require 'init-projects)
(require 'init-workspace)

;;; Memex

;; TODO: move after syntaxes? org-mode is heavy

(require 'init-org)
(require 'init-notes)
(require 'init-notes-denote)

;;; Language/syntax support

(require 'init-lisp)
(require 'init-lsp)

(require 'init-lang-html)
(require 'init-lang-json)
(require 'init-lang-lua)
(require 'init-lang-nix)
(require 'init-lang-php)
(require 'init-lang-yaml)
(require 'init-lang-misc)

;; FIXME: this is lang support, not integration -- rename to `init-lang-nu'
(require 'init-shell-nu)

;;; Miscellaneous

(require 'init-tools)
(require 'init-ledger)
(require 'init-fun)


;;
;;; Postlude
;;

(defun +maybe-start-server ()
  "Auto-start Emacs daemon if not already running."
  (require 'server)
  (unless (and (fboundp 'server-running-p)
               (server-running-p))
    (server-start)))
(add-hook 'elpaca-after-init-hook #'+maybe-start-server)

;; unfortunately
(defun cmx-after-init-restart-yabai-h ()
  "Restart the yabai service after Elpaca initialization is complete."
  (after! [exec-path-from-shell]
    (async-shell-command "yabai --restart-service")))
(when (and +gui-p +sys-mac-p)
  (add-hook 'elpaca-after-init-hook #'cmx-after-init-restart-yabai-h))

;; Load custom file after all packages have loaded.
(when (file-exists-p custom-file)
  (add-hook 'elpaca-after-init-hook
            (lambda () (load custom-file 'noerror))))

;; Wait for all packages to initialize in non-interactive mode.
(when noninteractive
  (elpaca-wait))

(provide 'init)
;;; init.el ends here
