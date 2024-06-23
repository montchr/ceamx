;;; init.el --- Initialize Ceamx  -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@proton.me>

;; Author: Chris Montgomery <chmont@proton.me>
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
;;; Code:

(require 'cl-lib)

;; Core variables
(require 'ceamx-paths)
(require 'ceamx-keymaps)

;; Core functions and macros
(require 'ceamx-lib)

(setq-default user-full-name "Chris Montgomery"
              user-mail-address "chmont@proton.me")

(add-to-list 'load-path ceamx-site-lisp-dir)
(prependq! load-path (ceamx-subdirs ceamx-site-lisp-dir))
(defgroup ceamx nil
  "User-configurable options for Ceamx."
  ;; TODO: is this group appropriate?
  :group 'file)
(defcustom ceamx-load-custom-file nil
  "Whether to load the user `custom-file' (custom.el)."
  :group 'ceamx
  :type '(boolean))
(require 'config-env)

;; TODO: see bbatsov/prelude for prior art
(when +sys-wsl-p
  (require 'lib-env-wsl))
(require 'on)
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" ceamx-packages-dir))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                       :ref nil :depth 1
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
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                    (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
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
(define-keymap :keymap ceamx-packages-map
  "f" #'elpaca-fetch-all
  "m" #'elpaca-merge-all
  "t" #'elpaca-try)

(keymap-set ceamx-session-map "p" '("Packages" . ceamx-packages-map))
(keymap-global-set "C-c q" ceamx-session-map)
(add-hook 'elpaca-after-init-hook #'ceamx-after-init-hook)
(add-hook 'elpaca-after-init-hook #'ceamx-emacs-startup-hook)
(require 'config-buffer)

(def-hook! ceamx-register-read-only-buffers-h ()
  'ceamx-after-init-hook
  "Use read-only buffers for files in some directories.
The affected directories are listed in `ceamx-buffer-read-only-dirs-list'"

  ;; Define a read-only directory class
  (dir-locals-set-class-variables
   'read-only
   '((nil . ((buffer-read-only . t)))))

  ;; Associate directories with the read-only class
  (dolist (dir ceamx-buffer-read-only-dirs-list)
    (dir-locals-set-directory-class (file-truename dir) 'read-only)))
(require 'ceamx-paths)

;; These must be set prior to package load.
(setq no-littering-etc-directory ceamx-etc-dir)
(setq no-littering-var-directory ceamx-var-dir)

(elpaca no-littering
  (require 'no-littering))
(defun +elpaca-unload-seq (e)
  "Unload the builtin version of `seq' and continue the `elpaca' build E."
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))

(defun +elpaca-seq-build-steps ()
  "Update the `elpaca' build-steps to activate the latest version of the builtin `seq' package."
  (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                       elpaca--pre-built-steps
                     elpaca-build-steps))
          (list '+elpaca-unload-seq 'elpaca--activate-package)))

(elpaca `(seq :build ,(+elpaca-seq-build-steps)))
(elpaca jsonrpc
  (require 'jsonrpc))
(unless after-init-time
  (unload-feature 'eldoc t)
  (setq custom-delayed-init-variables '())
  (defvar global-eldoc-mode nil))

(elpaca eldoc
  (require 'eldoc)
  (global-eldoc-mode))
(unless after-init-time
  (when (featurep 'eglot)
    (unload-feature 'eglot)))

(elpaca eglot)
(unless after-init-time
  (when (featurep 'flymake)
    (unload-feature 'flymake)))

(elpaca flymake)
(unless after-init-time
  (when (featurep 'org)
    (unload-feature 'org)))

(elpaca (org :autoloads "org-loaddefs.el"))
(elpaca use-package)
(elpaca elpaca-use-package
  (elpaca-use-package-mode))
(setopt use-package-always-ensure t)
(elpaca-wait)
(setopt use-package-expand-minimally nil)
(when (bound-and-true-p init-file-debug)
  (require 'use-package)
  (setopt use-package-expand-minimally nil)
  (setopt use-package-verbose t)
  (setopt use-package-compute-statistics t))
(elpaca blackout
  (require 'blackout))
(elpaca-wait)
(package! gcmh
  (blackout 'gcmh-mode)
  (add-hook 'ceamx-emacs-startup-hook #'gcmh-mode))
;; FIXME: remove or alias (`##' is very difficult to search for)
(use-package llama) ;  `##' lambda shorthand =>
                                        ;  <https://git.sr.ht/~tarsius/llama>

(use-package f)
(package! esup)
(after! esup
  (setq esup-depth 0))

;; Increase number of messages saved in log.
(setq message-log-max 10000)

;; Unbind `suspend-frame'.
(global-unset-key (kbd "C-x C-z"))

;; "A second, case-insensitive pass over `auto-mode-alist' is time wasted."
(setopt auto-mode-case-fold nil)

;; Prevent Emacs from pinging domain names unexpectedly.
(setopt ffap-machine-p-known 'reject)
(require 'init-env)
(require 'init-input-methods)

;; Site-specific configuration, to be ignored by version control.
(require 'site-config (file-name-concat user-emacs-directory "site-config") t)

(require 'init-secrets)
;;;; Displays + Appearance

;; Load configuration settings for conditional loading.
(require 'config-ui)

(require 'init-env-tty)

(require 'init-ui)

(when (display-graphic-p)
  (require 'init-ui-graphical))
;;;; Dashboard

(require 'init-dashboard)
;;;; Keyboard support

(require 'init-keys)
(require 'init-keys-which-key)
(require 'init-keys-meow)
;;;; Windows

(require 'init-window)
(require 'init-buffer)

;; FIXME: load earlier / in another section
(require 'init-history)
;;;; Text Expansion

(require 'init-abbrevs)
;;;; Completions and Selections

(require 'init-search)
(require 'init-completion)
;;;; Help

(require 'init-help)
;;;; Actions

(require 'init-embark)

;; Projects / Files
(require 'init-project)
(require 'init-vcs)
(require 'init-files)
(require 'init-dired)
;;;; Workspaces + activities + contexts

(require 'init-workspace)
;;;; Editing

(require 'init-editor)
(require 'init-writing)
(require 'init-templates)
;;;; Outlines & Memex

(require 'init-outline)
(require 'init-org)
(require 'init-notes)
(require 'init-notes-denote)
;;;; Linting

(require 'init-flymake)
;; (require 'init-flycheck)
;;;; Tree-Sitter

(require 'init-treesitter)
;;;; Language/syntax support

(require 'config-prog)
(require 'lib-prog)

(require 'init-prog)
(require 'init-lisp)
(require 'init-lsp)

(require 'init-lang-data)
(require 'init-lang-elisp)
(require 'init-lang-html)
(require 'init-lang-js)
(require 'init-lang-lua)
(require 'init-lang-markdown)
(require 'init-lang-nix)
(require 'init-lang-php)
(require 'init-lang-shell)
(require 'init-lang-misc)

;; FIXME: this is lang support, not integration -- rename to `init-lang-nu'
(require 'init-shell-nu)
;;;; Miscellaneous

(require 'init-tools)

(require 'init-term)
(require 'init-eww)
(require 'init-printing)

(require 'init-fun)

(require 'init-controls)

(setopt hippie-expand-verbose t
        hippie-expand-dabbrev-skip-space t)
;; These are mostly from the default value, for visibility.
(setopt hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name

          ;; Remove `try-expand-all-abbrevs' to disable automatic abbrev expansion.
          try-expand-all-abbrevs

          try-expand-list

          ;; TODO: enable for shell modes only?
          ;; try-expand-line

          try-expand-dabbrev            ; see: `dabbrev-expand'
          try-expand-dabbrev-all-buffers
          ;; try-expand-dabbrev-from-kill

          ;; Redundant with `completion-at-point'... *except* in the literate
          ;; config file, where elisp symbols won't normally be available.
          ;; TODO: enable for config.org
          ;; try-complete-lisp-symbol-partially ; before `try-complete-lisp-symbol'
          ;; try-complete-lisp-symbol ; after `try-complete-lisp-symbol-partially'
          ))
(package! persistent-scratch
  ;; `with-demoted-errors' or `ignore-errors' wrapper is recommended by project
  ;; readme, as there will be an error if the autosave file does not exist.
  (with-demoted-errors
    (persistent-scratch-autosave-mode 1)))
(keymap-set ceamx-launch-map "s" #'scratch-buffer)
(define-keymap :keymap ceamx-session-map
  "q" #'save-buffers-kill-emacs
  "Q" #'kill-emacs)
(defun ceamx/maybe-start-server ()
  "Allow this Emacs process to act as server process if not already running."
  (require 'server)
  (unless (and (fboundp 'server-running-p)
               (server-running-p))
    (server-start)))
(add-hook 'ceamx-emacs-startup-hook #'ceamx/maybe-start-server)
(when (and (display-graphic-p) +sys-mac-p)
  (def-hook! ceamx-after-init-restart-yabai-h ()
    'ceamx-after-init-hook
    "Restart the yabai service after init."
    (after! exec-path-from-shell
      (async-shell-command "yabai --restart-service"))))
(defun ceamx/load-custom-file ()
  "Load the user `custom-file'."
  (interactive)
  (when (file-exists-p custom-file)
    (load custom-file 'noerror)))
(add-hook 'ceamx-after-init-hook #'ceamx/load-custom-file)

(provide 'init)
;;; init.el ends here
