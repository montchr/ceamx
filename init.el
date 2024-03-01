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

;; TODO: <https://github.com/Genivia/ugrep#using-ugrep-within-emacs>
;;       <https://manueluberti.eu/posts/2022-08-07-emacs-ugrep/#fn:3>
;;       <https://manueluberti.eu/posts/2023-10-01-embark-ugrep/>
;;       <https://manueluberti.eu/posts/2021-09-10-rgrep-and-vc-git-grep/>
;; TODO: <https://elpa.gnu.org/packages/xr.html>
;; TODO: <https://github.com/leoliu/easy-kill>

;;; Code:

(require 'cl-lib)

(require 'ceamx-paths)

(require 'lib-common)

(defgroup ceamx nil
  "User-configurable options for Ceamx."
  ;; TODO: is this group appropriate?
  :group 'file)

(defcustom ceamx-load-custom-file nil
  "Whether to load the user `custom-file' (custom.el)."
  :group 'ceamx
  :type '(boolean))

(setq-default user-full-name "Chris Montgomery"
              user-mail-address "chris@cdom.io")

(require 'init-benchmarking)

;;
;;; Load environment-related constants

(require 'config-env)
;; TODO: see bbatsov/prelude for prior art
(when +sys-wsl-p
  (require 'lib-env-wsl))

;;
;;; Initialize packages

;; Third-party package managers should be configured in init.el directly instead
;; of within a `require'd file so that they may be re-initialized properly.

;; Add site-lisp directory tree to load path.
(add-to-list 'load-path ceamx-site-lisp-dir)
(prependq! load-path (subdirs! ceamx-site-lisp-dir))

;;;; Preface


;;;; Bootstrap

(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" ceamx-packages-dir))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                       :ref nil
                       :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                       :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;;; Configure elpaca use-package integration

(setopt use-package-always-ensure t)

(elpaca use-package)

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(elpaca-wait)

;;;; Improve `use-package' debuggability if necessary

(setopt use-package-expand-minimally nil)
(when (bound-and-true-p init-file-debug)
  (require 'use-package)
  (setopt use-package-expand-minimally nil)
  (setopt use-package-verbose t)
  (setopt use-package-compute-statistics t))

(add-hook 'elpaca-after-init-hook #'ceamx-after-init-hook)
(add-hook 'elpaca-after-init-hook #'ceamx-emacs-startup-hook)

;;;; Essential storage path cleanup for features/packages

;; <https://github.com/emacscollective/no-littering/>

(use-package no-littering
  :demand t
  :init
  (setq no-littering-etc-directory ceamx-etc-dir)
  (setq no-littering-var-directory ceamx-var-dir))

(elpaca-wait)

;;;; Latest versions of builtin libraries

;; The builtin libraries must be unloaded before loading the newer version. This
;; will prevent warnings like "eldoc loaded before Elpaca activation".

;; `magit' requires a more recent version of `seq' than the version included in
;; Emacs 29.
;;
;; FIXME: prevent "seq loaded before Elpaca activation" warning
;;        but `unload-feature' causes error since seq is used earlier in init
(use-package seq
  :ensure t
  :demand t)

;; Required by (and originally extracted from) `eglot'.
(use-package jsonrpc
  :ensure t
  :demand t)

;; Unfortunately, this requires a delicate workaround:
;; <https://github.com/progfolio/elpaca/issues/236#issuecomment-1879838229>
(use-package eldoc
  :ensure t
  :demand t

  :preface
  (unload-feature 'eldoc t)
  (setq custom-delayed-init-variables '())
  (defvar global-eldoc-mode nil)

  :config
  (global-eldoc-mode))

;;;;; Install the latest version of the builtin `eglot' package

(use-package eglot
  :after (eldoc jsonrpc)
  :preface
  (when (featurep 'eglot)
    (unload-feature 'eglot)))

;;;;; Install the latest version of Org-Mode (`org')

(use-package org
  :preface
  (when (featurep 'org)
    (unload-feature 'org)))

;;;;; Ensure the previously-queued package requests have completed

(elpaca-wait)

;;;; Initialize miscellaneous packages adding `use-package' keywords

;; NOTE: `blackout' is still useful even without `use-package'
(use-package blackout
  :demand t)

(elpaca-wait)

;;;; Run garbage collection on idle

;; <https://gitlab.com/koral/gcmh>
;; <https://akrl.sdf.org/>

;; During normal use, the GC threshold will be set to a high value.
;; When idle, GC will be triggered with a low threshold.

(use-package gcmh
  :blackout
  :commands (gcmh-mode)
  :init
  (add-hook 'ceamx-emacs-startup-hook #'gcmh-mode)
  (setopt gcmh-high-cons-threshold (* 16 1024 1024)))

;;
;;; Local packages

(use-feature! on
  :demand t)

(elpaca-wait)

;;
;;; Libraries

;; FIXME: remove or alias (`##' is very difficult to search for)
(use-package llama) ;  `##' lambda shorthand =>
                                        ;  <https://git.sr.ht/~tarsius/llama>

(use-package f)

(require 'lib-common)

(when (display-graphic-p)
  (require 'lib-gui))
(require 'lib-files)

;;
;;; Configuration

;; Generally sorted in order of likelihood of first user interaction.

;; An interaction can be one-way or two-way:
;; keep in mind that perception is still a (one-way) interaction,
;; so legibility of displayed information is important immediately,
;; even if the interface cannot respond to input.

(require 'init-defaults)

(require 'init-env)

;; Site-specific configuration, to be ignored by version control.
(require 'site-config (file-name-concat user-emacs-directory "site-config") t)

(require 'init-secrets)

;;;; Displays + Appearance

;; TODO: re-enable
;; (require 'init-frame-hooks)
(require 'init-env-tty)
(require 'init-frame)

;; Load configuration settings for conditional loading.
(require 'config-ui)

(require 'init-ui)

(require 'init-ui-theme)
(require 'init-ui-ef-themes)
(require 'init-ui-modus-themes)

(when (display-graphic-p)
  (require 'init-ui-font))

(require 'init-ui-modeline)

;;;;; Integrations for visual consistency

(require 'init-ui-circadian)

(require 'init-after-ui)

;;;; Keyboard support

(require 'config-keys)

(require 'init-keys)
(require 'init-keys-which-key)
(require 'init-keys-meow)

;;;; Windows

(require 'init-window)
(require 'init-window-popups)
(require 'init-buffer)

;; FIXME: load earlier / in another section
(require 'init-history)

;;;; Dashboard

(require 'init-dashboard)

;;;; Selection

(require 'init-selection-vertico)
(require 'init-selection-orderless)
(require 'init-selection-marginalia)
(require 'init-selection-consult)

(require 'init-search)

;;;; Completion-At-Point

(require 'config-completion)
(require 'init-completion)

;;;; Help

(require 'init-help)

;;;; Actions

(require 'init-embark)
(require 'init-wgrep)

;; Projects / Files
(require 'init-project)
;; (require 'init-project-projectile)
(require 'init-vcs)
(require 'init-vcs-magit)
(require 'init-vcs-forge)
(require 'init-files)
(require 'init-dired)

;;;; Workspaces + activities + contexts

(require 'init-workspace)

;;;; Editing

(require 'init-editor)
(require 'init-templates)

;;;; Memex

;; TODO: move after syntaxes? org-mode is heavy

(require 'init-org)
(require 'init-notes)
(require 'init-notes-denote)

;;;; Linting

(require 'init-flycheck)

;;;; Tree-Sitter

(require 'init-treesitter)

;;;; Language/syntax support

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
(require 'init-lang-shell)
(require 'init-lang-yaml)
(require 'init-lang-misc)

;; FIXME: this is lang support, not integration -- rename to `init-lang-nu'
(require 'init-shell-nu)

(require 'init-eglot)
;; (require 'init-lsp)


;;;; Miscellaneous

(require 'init-tools)
(require 'init-tools-ai)
(require 'init-tools-ledger)
(require 'init-tools-pdf)

(require 'init-term)

(require 'init-printing)

(require 'init-fun)

;;;; Keybindings

(require 'init-keys-bindings)

;;
;;; Postlude

(def-hook! ceamx-maybe-start-emacs-server-h () 'ceamx-after-init-hook
  "Auto-start Emacs daemon if not already running."
  (require 'server)
  (unless (and (fboundp 'server-running-p)
            (server-running-p))
    (server-start)))

;; unfortunately
(when (and +gui-p +sys-mac-p)
  (def-hook! ceamx-after-init-restart-yabai-h () 'ceamx-after-init-hook
             "Restart the yabai service after init."
             (after! [exec-path-from-shell]
               (async-shell-command "yabai --restart-service"))))

;; Optionally load custom file after all packages have loaded.
(when (and ceamx-load-custom-file
           (file-exists-p custom-file))
  (def-hook! ceamx-after-init-load-custom-file-h ()
    'ceamx-after-init-hook
    "Load the user `custom-file'.
Keep in mind that the custom file is ignored in version control."
    (load custom-file 'noerror)))

(provide 'init)
;;; init.el ends here
