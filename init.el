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

;; FIXME: cannot load some packages (namely `spacious-padding' in tty session)

;;; Sources:

;; - <https://git.sr.ht/~protesilaos/dotfiles/tree/e21affc0153e556e06a28813efb252c7757b6aff/item/emacs/.emacs.d/init.el>

;;; Code:

(defgroup ceamx nil
  "User-configurable options for Ceamx."
  :group 'file)

;; TODO: describe
(defcustom cmx-init-debug-flag nil
  "Whether to enable initialization debug flag (this is it)."
  :group 'ceamx
  :type '(boolean))

;;; Configure load path.
(dolist (subdir '("autoloads" "lisp" "lisp/lib"))
  (let ((dir (expand-file-name subdir user-emacs-directory)))
    (add-to-list 'load-path dir)))

;;; Configure customization file.
(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))

;;; Define default user identity.
(setq-default user-full-name "Chris Montgomery"
              user-mail-address "chris@cdom.io")

;;; Load environment-related constants.
(require 'config-env)

;;
;;; Package initialization
;;

(require 'package)

;; Package manifest (update via `package-quickstart-refresh' after changes).
;; TODO: uncomment when stable
;; (setq package-quickstart t)
;; (setq package-quickstart-file "ceamx-packages.el")

(add-hook 'package-menu-mode-hook #'hl-line-mode)

;;; `use-package'
;;  <https://www.gnu.org/software/emacs/manual/html_mono/use-package.html>
;;  <https://github.com/jwiegley/use-package>
;;  TODO: make sure we are using the version installed by nixpkgs, not the builtin version
;;        same goes for modus-themes and others
(eval-when-compile
  (require 'use-package)

  ;;;; Initialize keyword extensions.
  (use-package blackout :demand t))

(require 'lib-package)

(use-feature! use-package
  :config
  ;; When non-nil, improves performance and effectiveness of byte-compilation,
  ;; but decreases introspectability.
  ;; TODO: switch to enabled when ready for byte-comp
  (setopt use-package-expand-minimally nil)

  ;; Support for Emacs init introspection.
  (when cmx-init-debug-flag
    (setopt use-package-expand-minimally nil)
    ;; NOTE: Requires that the `use-package' library is explicitly `require'd in
    ;;       files where its macro is invoked, else no output.
    ;;       See docstring for more info.
    (setopt use-package-verbose t)
    ;; NOTE: Requires explicit `require', else errors re: undefined variables.
    (setopt use-package-compute-statistics t)))

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

;; FIXME: needs autoloads of its own -- missing dependencies (esp. evil)
;;        another reason some kind of module-like directory organization
;;        with its own set of autoloads would allow for better encapsulation
;; (require 'ceamx-autoloads)

;;; Add site-lisp directory tree to load path.
(add-to-list 'load-path cmx-site-lisp-dir)
(prependq! load-path (subdirs! cmx-site-lisp-dir))

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

(require 'config-ui)
(require 'init-ui)

;;;; Theme
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
;; TODO: why not tty?
(when (display-graphic-p)
  (pcase cmx-modeline-provider
    (`nano           (require 'init-ui-modeline-nano))
    (`doom-modeline  (require 'init-ui-modeline-doom))
    (`telephone-line (require 'init-ui-modeline-telephone-line))))

;;;; Sidebar
;; TODO: figure out how to load as late as possible?
(require 'init-ui-treemacs)

;;; Keybindings
(require 'init-keys)
(defvar cmx-modal-keys-provider 'evil)
(pcase cmx-modal-keys-provider
  (`boon (require 'init-keys-boon))
  (`evil (require 'init-keys-evil))
  (`meow (require 'init-keys-meow)))
(require 'init-keys-bindings)

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
;; TODO: move to after lang modes, since it's less essential
(require 'init-lsp)

(require 'init-lang-data)
(require 'init-lang-emacs-lisp)
(require 'init-lang-html)
(require 'init-lang-json)
(require 'init-lang-lua)
(require 'init-lang-markdown)
(require 'init-lang-nix)
(require 'init-lang-php)
(require 'init-lang-yaml)
(require 'init-lang-misc)

;; FIXME: this is lang support, not integration -- rename to `init-lang-nu'
(require 'init-shell-nu)

;;; Linting
(require 'init-flycheck)

;;; Tree-Sitter
(when (and (require 'treesit nil t)
           (fboundp 'treesit-available-p)
           (treesit-available-p))
  (require 'init-treesitter))

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
(add-hook 'after-init-hook #'+maybe-start-server)

;; unfortunately
(defun cmx-after-init-restart-yabai-h ()
  "Restart the yabai service after Elpaca initialization is complete."
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
(when noninteractive
  (with-eval-after-load 'elpaca
    (elpaca-wait)))

(provide 'init)
;;; init.el ends here
