;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chris@cdom.io>

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

;; FIXME: cannot load some packages (namely `spacious-padding' in tty session)

;;; Sources:

;; - <https://git.sr.ht/~protesilaos/dotfiles/tree/e21affc0153e556e06a28813efb252c7757b6aff/item/emacs/.emacs.d/init.el>

;;; Code:

(defgroup ceamx nil
  "User-configurable options for Ceamx."
  :group 'file)

;;; Configure customization file.
(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))

;;; Define default user identity.
(setq-default user-full-name "Chris Montgomery"
              user-mail-address "chris@cdom.io")

;;; Load environment-related constants.
(require 'config-env)
;; TODO: see bbatsov/prelude for prior art
(when +sys-wsl-p
  (require 'lib-env-wsl))

;;
;;; Package initialization
;;

(require 'init-packages)

;;
;;; Libraries
;;

;;; Latest versions of Emacs internals, required by some packages.
;;  Prevents Elpaca errors for packages that expect later versions available.
(use-package eldoc
  :defer t)
(use-package jsonrpc
  :defer t)

;; FIXME: remove or alias (`##' is very difficult to search for)
(use-package llama) ;  `##' lambda shorthand => <https://git.sr.ht/~tarsius/llama>

;;; Ceamx basic libraries
(require 'lib-common)
(when (display-graphic-p)
  (require 'lib-gui))
(require 'lib-files)

;; FIXME: needs autoloads of its own -- missing dependencies (esp. evil)
;;        another reason some kind of module-like directory organization
;;        with its own set of autoloads would allow for better encapsulation
;; (require 'ceamx-autoloads)

;;; Add site-lisp directory tree to load path.
(add-to-list 'load-path cmx-site-lisp-dir)
(prependq! load-path (subdirs! cmx-site-lisp-dir))

;;; site-lisp packages
;; TODO: nothing here, placeholder

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

;; FIXME: for science: prevent startup warnings?
;; (require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-frame)

;; TODO: no need to import `config-ui' i think?
(require 'config-ui)
(require 'init-ui)

;;;; Theme
(require 'init-ui-theme)
;; TODO: probably not for tty?
(pcase cmx-theme-family
  ('ef
   (require 'init-ui-ef-themes))
  ('modus
   (require 'init-ui-modus-themes))
  ('nano
   ;; NOTE: this is probably very broken -- so is the upstream project :/
   (require 'init-ui-nano-theme)))


;;;; Typography + Iconography
(when (display-graphic-p)
  (require 'init-ui-font))
;; Icons *can* work in non-graphical environments, so packages are
;; enabled/configured based on `display-graphic-p' within.
(require 'init-ui-icons)

;;;; Modeline
(require 'config-ui)
(require 'init-ui-modeline)
(pcase cmx-modeline-provider
  ('doom (require 'init-ui-modeline-doom))
  ('nano (require 'init-ui-modeline-nano))
  ('telephone (require 'init-ui-modeline-telephone-line)))

(require 'init-after-ui)

;;; Workspace
(require 'init-workspace)

;;;; Sidebar
;; TODO: should not be considered "ui" -- or rather, "ui" should mean "appearance"
;; TODO: figure out how to load as late as possible?
(require 'init-ui-treemacs)

;;;; Keyboard support

(require 'config-keys)
(require 'init-keys)

;; Ideally `which-key' would be loaded later to ensure the accuracy of its
;; menus, but `meow' does not seem to recognize `which-key' as being enabled
;; when it loads, which means that it uses its (broken) custom functionality to
;; do the same as `which-key'.
(require 'init-keys-which-key)

(pcase ceamx-keybinding-scheme
  ('evil (require 'init-keys-evil))
  ('meow (require 'init-keys-meow)))

;;; Window
(require 'init-window)
(require 'init-buffer)
(require 'init-history)

;;; Selection
(require 'init-selection-vertico)
(require 'init-selection-orderless)
(require 'init-selection-marginalia)
(require 'init-selection-consult)

;;; Completion-At-Point
(require 'config-completion)
(require 'init-completion)
;; TODO: this
;; (pcase cmx-completion-at-point-ui
;;   (`lsp-bridge (require 'init-lsp-bridge))
;;   ;; FIXME: rename/restruct feature to be specific to corfu, separate generalities
;;   (`corfu      (require 'init-completion)))

;;; Help
(require 'init-help)

;;; Actions
;; TODO: move these towards the end since they should be deferred until needed
(require 'init-embark)
(require 'init-wgrep)

;; Projects / Files
(require 'init-projects)
(require 'init-diff)
(require 'init-vcs)
(require 'init-vcs-magit)
(require 'init-files)
(require 'init-dired)

;;; Editing
(require 'init-editor)
(require 'init-templates)

;;; Memex

;; TODO: move after syntaxes? org-mode is heavy

(require 'init-org)
(require 'init-notes)
(require 'init-notes-denote)

;;; Language/syntax support

(require 'init-prog)
(require 'init-lisp)

(require 'init-lang-data)
(require 'init-lang-emacs-lisp)
(require 'init-lang-html)
(require 'init-lang-js)
(require 'init-lang-json)
(require 'init-lang-lua)
(require 'init-lang-markdown)
(require 'init-lang-nix)
(require 'init-lang-php)
(require 'init-lang-yaml)
(require 'init-lang-misc)

;; FIXME: this is lang support, not integration -- rename to `init-lang-nu'
(require 'init-shell-nu)

(require 'init-eglot)
;; (require 'init-lsp)

;;; Linting
(require 'init-flycheck)

;;; Tree-Sitter
(require 'init-treesitter)

;;; Miscellaneous
(require 'init-secrets)
(require 'init-tools)
(require 'init-ledger)
(require 'init-fun)
;; TODO: nothing here yet
;; (require 'init-news)

;;; Keybindings
(require 'init-keys-bindings)

;;
;;; Postlude
;;

(defun +maybe-start-server ()
  "Auto-start Emacs daemon if not already running."
  (require 'server)
  (unless (and (fboundp 'server-running-p)
            (server-running-p))
    (server-start)))
(add-hook 'after-init-hook #'+maybe-start-server)

;; unfortunately
(defun cmx-after-init-restart-yabai-h ()
  "Restart the yabai service after init."
  (after! [exec-path-from-shell]
    (async-shell-command "yabai --restart-service")))
(when (and +gui-p +sys-mac-p)
  (add-hook 'after-init-hook #'cmx-after-init-restart-yabai-h))

;; Load custom file after all packages have loaded.
(when (file-exists-p custom-file)
  (defun cmx-load-custom-file-after-init-h ()
    (load custom-file 'noerror))
  (add-hook 'after-init-hook #'cmx-load-custom-file-after-init-h))

;; Wait for all packages to initialize in non-interactive mode.
(when (and noninteractive
           (fboundp 'elpaca-wait))
  (elpaca-wait))

(provide 'init)
;;; init.el ends here
