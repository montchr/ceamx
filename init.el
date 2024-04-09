;;; init.el --- Initialize Ceamx  -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chris@cdom.io>

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
;;; Load environment-related constants

(require 'config-env)
;; TODO: see bbatsov/prelude for prior art
(when +sys-wsl-p
  (require 'lib-env-wsl))
;;; Initialize packages

;; Third-party package managers should be configured in init.el directly instead
;; of within a `require'd file so that they may be re-initialized properly.

;; Add site-lisp directory tree to load path.
(add-to-list 'load-path ceamx-site-lisp-dir)
(prependq! load-path (ceamx-subdirs ceamx-site-lisp-dir))

;;;; Preface


;;;; Bootstrap

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

;;;; Use latest versions of some Emacs builtins to satisfy bleeding-edge packages

;; Installing the latest development versions of `eglot' and `magit' (for
;; example) comes with the significant caveat that their dependencies often
;; track the latest versions of builtin Emacs libraries. Those can be installed
;; via GNU ELPA.
;;
;; Since core libraries like `seq' are often dependencies of many other packages
;; or otherwise loaded immediately (like `eldoc'), installation and activation
;; of the newer versions needs to happen upfront to avoid version conflicts and
;; mismatches. For example, we do not want some package loaded earlier in init
;; to think it is using the builtin version of `seq', while a package loaded
;; later in init uses a differnt version. I am not sure how realistic such a
;; scenario might be, or whether it would truly pose a problem, but the point is
;; that we should aim for consistency.
;;
;; Oftentimes, these builtins must be unloaded before loading the newer version.
;; This applies especially to core libraries like `seq' or the
;; enabled-by-default `global-eldoc-mode' provided by `eldoc', but not
;; `jsonrpc', since its functionality is specific to more niche features like
;; inter-process communication in the case of `eglot'.
;;
;; A feature must only be unloaded once, *before* loading the version installed
;; by Elpaca. Normally, that is not an issue because the init file is only
;; loaded once on session startup. But when you are re-loading the init file
;; inside a running session, you'd actually end up unloading the version that
;; Elpaca loaded. To prevent that, the unloading should happen only once --
;; during session startup -- so we check for a non-nil value of `after-init-time'.
;;
;; I don't understand why the Elpaca-installed feature/package only seems to be
;; loaded during the initial session startup? Unless the unloading happens
;; conditionally based on `after-init-time' as described above, every time the
;; init file is reloaded and `elpaca-process-queues' runs in
;; `+auto-tangle-reload-init-h', I get a bunch of errors (not warnings!) about
;; `eglot' and `org' as missing dependencies.

;;;;; Install the latest version of `seq' builtin library, carefully

;; `magit' requires a more recent version of `seq' than the version included in
;; Emacs 29.

;; Requires special care because unloading it can make other libraries freak out.
;; <https://github.com/progfolio/elpaca/issues/216#issuecomment-1868444883>

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

;;;;; Install the latest version of `jsonrpc' builtin library

;; Required by (and originally extracted from) `eglot'.

(elpaca jsonrpc
  (require 'jsonrpc))

;;;;; Install the latest version of `eldoc' builtin library, carefully

;; Required by `eglot'.

;; `eldoc' requires a delicate workaround to avoid catastrophy.
;; <https://github.com/progfolio/elpaca/issues/236#issuecomment-1879838229>

(unless after-init-time
  (unload-feature 'eldoc t)
  (setq custom-delayed-init-variables '())
  (defvar global-eldoc-mode nil))

(elpaca eldoc
  (require 'eldoc)
  (global-eldoc-mode))

;;;;; Install the latest version of the builtin `eglot' package

(unless after-init-time
  (when (featurep 'eglot)
    (unload-feature 'eglot)))

(elpaca eglot)

;;;;; Install the latest version of Org-Mode (`org')

(unless after-init-time
  (when (featurep 'org)
    (unload-feature 'org)))

(elpaca (org :autoloads "org-loaddefs.el"))

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

;;;; Treat file-visiting-buffers in the package directory as read-only

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

;;; Site-lisp packages

(require 'on)


(elpaca-wait)

;;; Libraries

;; FIXME: remove or alias (`##' is very difficult to search for)
(use-package llama) ;  `##' lambda shorthand =>
                                        ;  <https://git.sr.ht/~tarsius/llama>

(use-package f)

(require 'lib-common)

(require 'lib-files)
(require 'lib-elisp)
;;; Configuration

;; Increase number of messages saved in log.
(setq message-log-max 10000)

;; Unbind `suspend-frame'.
;; TODO: provide more context
(global-unset-key (kbd "C-x C-z"))

;; "A second, case-insensitive pass over `auto-mode-alist' is time wasted."
(setopt auto-mode-case-fold nil)

;; Prevent Emacs from pinging domain names unexpectedly.
(setopt ffap-machine-p-known 'reject)

;;;; Environment

(require 'init-env)
(require 'init-input-methods)

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
(require 'init-ui-modeline-doom)

;;;;; Integrations for visual consistency

(require 'init-ui-circadian)

(require 'init-after-ui)

;;;; Keyboard support

(require 'init-keys)
(require 'init-keys-which-key)
(require 'init-keys-meow)

;;;; Windows

(require 'init-window)
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

(require 'init-completion)
(require 'init-abbrevs)

;;;; Help

(require 'init-help)

;;;; Actions

(require 'init-embark)
(require 'init-wgrep)

;; Projects / Files
(require 'init-project)
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
(require 'init-lang-elisp)
(require 'init-lang-html)
(require 'init-lang-js)
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
(require 'init-eww)
(require 'init-printing)

(require 'init-fun)

(require 'init-controls)

;;; Postlude

;; FIXME: causes some errors / inconsistencies
;; (def-hook! ceamx-maybe-start-emacs-server-h () 'ceamx-after-init-hook
;;   "Auto-start Emacs daemon if not already running."
;;   (require 'server)
;;   (unless (and (fboundp 'server-running-p)
;;             (server-running-p))
;;     (server-start)))

;; unfortunately
(when (and +gui-p +sys-mac-p)
  (def-hook! ceamx-after-init-restart-yabai-h () 'ceamx-after-init-hook
    "Restart the yabai service after init."
    (after! exec-path-from-shell
      (async-shell-command "yabai --restart-service"))))

;; Optionally load custom file after all packages have loaded.
;; (when (and ceamx-load-custom-file
;;            (file-exists-p custom-file))
;;   (def-hook! ceamx-after-init-load-custom-file-h ()
;;     'ceamx-after-init-hook
;;     "Load the user `custom-file'.
;; Keep in mind that the custom file is ignored in version control."
;;     (load custom-file 'noerror)))

(provide 'init)
;;; init.el ends here
