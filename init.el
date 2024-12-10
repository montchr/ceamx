;;; init.el --- Initialize Ceamx  -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>
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

;; Dependencies


;; [[file:config.org::*Dependencies][Dependencies:1]]
(require 'cl-lib)

;; Core variables
(require 'ceamx-paths)
(require 'ceamx-keymaps)

;; Core functions and macros
(require 'ceamx-lib)
;; Dependencies:1 ends here

;; Configure default identity


;; [[file:config.org::*Configure default identity][Configure default identity:1]]
(setq-default user-full-name "Chris Montgomery"
              user-mail-address "chmont@protonmail.com")
;; Configure default identity:1 ends here

;; Profiling

;; - source :: <https://github.com/progfolio/.emacs.d/blob/ed159dc6076664ad9976949d8cb3af8e86fe39d1/init.org#profiling>


;; [[file:config.org::*Profiling][Profiling:1]]
(add-hook 'ceamx-after-init-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract (current-time) before-init-time)))
                     gcs-done)))
;; Profiling:1 ends here

;; Add the =site-lisp= directory to ~load-path~


;; [[file:config.org::*Add the =site-lisp= directory to ~load-path~][Add the =site-lisp= directory to ~load-path~:1]]
(add-to-list 'load-path ceamx-site-lisp-dir)
(prependq! load-path (ceamx-subdirs ceamx-site-lisp-dir))
;; Add the =site-lisp= directory to ~load-path~:1 ends here

;; Initialize the =ceamx= user options


;; [[file:config.org::*Initialize the =ceamx= user options][Initialize the =ceamx= user options:1]]
(defgroup ceamx nil
  "User-configurable options for Ceamx."
  ;; TODO: is this group appropriate?
  :group 'file)
;; Initialize the =ceamx= user options:1 ends here

;; The user option to determine whether to load ~custom-file~


;; [[file:config.org::*The user option to determine whether to load ~custom-file~][The user option to determine whether to load ~custom-file~:1]]
(defcustom ceamx-load-custom-file nil
  "Whether to load the user `custom-file' (custom.el)."
  :group 'ceamx
  :type '(boolean))
;; The user option to determine whether to load ~custom-file~:1 ends here

;; Define variables describing the current environment-context :env:


;; [[file:config.org::*Define variables describing the current environment-context][Define variables describing the current environment-context:1]]
(require 'config-env)

;; TODO: see bbatsov/prelude for prior art
(when +sys-wsl-p
  (require 'lib-env-wsl))
;; Define variables describing the current environment-context:1 ends here

;; =site-lisp/on=: Define additional Emacs event hooks


;; [[file:config.org::*=site-lisp/on=: Define additional Emacs event hooks][=site-lisp/on=: Define additional Emacs event hooks:1]]
(require 'on)
;; =site-lisp/on=: Define additional Emacs event hooks:1 ends here

;; Set the Elpaca installer version


;; [[file:config.org::*Set the Elpaca installer version][Set the Elpaca installer version:1]]
(defvar elpaca-installer-version 0.8)
;; Set the Elpaca installer version:1 ends here

;; Show the Elpaca to its house


;; [[file:config.org::*Show the Elpaca to its house][Show the Elpaca to its house:1]]
(defvar elpaca-directory (expand-file-name "elpaca/" ceamx-packages-dir))
;; Show the Elpaca to its house:1 ends here

;; Summon the Elpaca

;; The installation code only needs to be changed when the Elpaca warns about an
;; installer version mismatch.

;; This should be copied verbatim from the Elpaca documentation, sans the
;; definitions for ~elpaca-installer-version~ and ~elpaca-directory~.


;; [[file:config.org::*Summon the Elpaca][Summon the Elpaca:1]]
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28)
      (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  (when-let* ((depth (plist-get order :depth)))
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
;; Summon the Elpaca:1 ends here

;; Bind some commonly-used package management commands :keybinds:


;; [[file:config.org::*Bind some commonly-used package management commands][Bind some commonly-used package management commands:1]]
(define-keymap :keymap ceamx-packages-map
  "f" #'elpaca-fetch-all
  "m" #'elpaca-merge-all
  "t" #'elpaca-try)

(keymap-set ceamx-session-map "p" '("Packages" . ceamx-packages-map))
;; Bind some commonly-used package management commands:1 ends here

;; TODO Move this global binding somewhere else... but where? :keybinds:


;; [[file:config.org::*Move this global binding somewhere else... but where?][Move this global binding somewhere else... but where?:1]]
(keymap-global-set "C-c q" ceamx-session-map)
;; Move this global binding somewhere else... but where?:1 ends here

;; Run our custom init and startup hooks on ~elpaca-after-init-hook~


;; [[file:config.org::*Run our custom init and startup hooks on ~elpaca-after-init-hook~][Run our custom init and startup hooks on ~elpaca-after-init-hook~:1]]
(add-hook 'elpaca-after-init-hook #'ceamx-after-init-hook)
(add-hook 'elpaca-after-init-hook #'ceamx-emacs-startup-hook)
;; Run our custom init and startup hooks on ~elpaca-after-init-hook~:1 ends here

;; Pretend file-visiting-buffers in the package directory are read-only


;; [[file:config.org::*Pretend file-visiting-buffers in the package directory are read-only][Pretend file-visiting-buffers in the package directory are read-only:1]]
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
;; Pretend file-visiting-buffers in the package directory are read-only:1 ends here

;; Encourage a ~no-littering~ policy for packages to artifice in the designated areas

;; - Website :: <https://github.com/emacscollective/no-littering/>

;; By default, Emacs features and many packages default to dumping their state
;; files into ~user-emacs-directory~.  This makes sense for the sake of visibility.
;; However, because E rarely thinks about any of those machine-generated and
;; non-human-friendly files, they may be effectively designated as clutter.  Ceamx
;; offloads these sanitation duties to the =no-littering= package because it works
;; effectively and almost-invisibly.

;; In some cases, especially for new packages / package features / targets, it may
;; be necessary to manage such configuration by hand.

;; Ceamx avoids ~use-package~ here so that:

;; - ~no-littering~ may be installed and loaded as early as possible
;; - the time-consuming invocations of ~elpaca-wait~ should be kept to the absolute minimum


;; [[file:config.org::*Encourage a ~no-littering~ policy for packages to artifice in the designated areas][Encourage a ~no-littering~ policy for packages to artifice in the designated areas:1]]
(require 'ceamx-paths)

;; These must be set prior to package load.
(setq no-littering-etc-directory ceamx-etc-dir)
(setq no-littering-var-directory ceamx-var-dir)

(elpaca no-littering
  (require 'no-littering))
;; Encourage a ~no-littering~ policy for packages to artifice in the designated areas:1 ends here

;; Install the latest version of ~seq~ builtin library, carefully

;; ~magit~ requires a more recent version of ~seq~ than the version included in
;; Emacs 29.

;; Requires special care because unloading it can make other libraries freak out.
;; <https://github.com/progfolio/elpaca/issues/216#issuecomment-1868444883>


;; [[file:config.org::*Install the latest version of ~seq~ builtin library, carefully][Install the latest version of ~seq~ builtin library, carefully:1]]
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
;; Install the latest version of ~seq~ builtin library, carefully:1 ends here

;; Install the latest version of the builtin ~jsonrpc~ library

;; Required by (and originally extracted from) ~eglot~.


;; [[file:config.org::*Install the latest version of the builtin ~jsonrpc~ library][Install the latest version of the builtin ~jsonrpc~ library:1]]
(elpaca jsonrpc
  (require 'jsonrpc))
;; Install the latest version of the builtin ~jsonrpc~ library:1 ends here

;; Install the latest version of the ~eldoc~ builtin library, carefully

;; Required by ~eglot~.

;; ~eldoc~ requires a delicate workaround to avoid catastrophy
;; <https://github.com/progfolio/elpaca/issues/236#issuecomment-1879838229>



;; [[file:config.org::*Install the latest version of the ~eldoc~ builtin library, carefully][Install the latest version of the ~eldoc~ builtin library, carefully:1]]
(unless after-init-time
  (unload-feature 'eldoc t)
  (setq custom-delayed-init-variables '())
  (defvar global-eldoc-mode nil))

(elpaca eldoc
  (require 'eldoc)
  (global-eldoc-mode))
;; Install the latest version of the ~eldoc~ builtin library, carefully:1 ends here

;; Install the latest version of the builtin ~eglot~ package


;; [[file:config.org::*Install the latest version of the builtin ~eglot~ package][Install the latest version of the builtin ~eglot~ package:1]]
(unless after-init-time
  (when (featurep 'eglot)
    (unload-feature 'eglot)))

(elpaca eglot)
;; Install the latest version of the builtin ~eglot~ package:1 ends here

;; Install the latest version of the builtin ~flymake~ package


;; [[file:config.org::*Install the latest version of the builtin ~flymake~ package][Install the latest version of the builtin ~flymake~ package:1]]
(unless after-init-time
  (when (featurep 'flymake)
    (unload-feature 'flymake)))

(elpaca flymake)
;; Install the latest version of the builtin ~flymake~ package:1 ends here

;; Install the latest version of Org-Mode


;; [[file:config.org::*Install the latest version of Org-Mode][Install the latest version of Org-Mode:1]]
(unless after-init-time
  (when (featurep 'org)
    (unload-feature 'org)))

(elpaca (org :autoloads "org-loaddefs.el"))
;; Install the latest version of Org-Mode:1 ends here

;; Install the latest version of Use-Package


;; [[file:config.org::*Install the latest version of Use-Package][Install the latest version of Use-Package:1]]
(elpaca use-package)
;; Install the latest version of Use-Package:1 ends here

;; Integrate Elpaca and Use-Package


;; [[file:config.org::*Integrate Elpaca and Use-Package][Integrate Elpaca and Use-Package:1]]
(elpaca elpaca-use-package
  (elpaca-use-package-mode))
;; Integrate Elpaca and Use-Package:1 ends here

;; Use-Package: Ensure package installation by default

;; Equivalent to manually specifying =:ensure t= in each ~use-package~ expression.


;; [[file:config.org::*Use-Package: Ensure package installation by default][Use-Package: Ensure package installation by default:1]]
(setopt use-package-always-ensure t)
;; Use-Package: Ensure package installation by default:1 ends here

;; Elpaca-Wait № 1: finish processing current queue :wait:

;; Reason:

;; - Continuing otherwise will result in race conditions on the definition of storage paths
;; - ~use-package~ must be loaded for byte-compilation checks in [[*Configure ~use-package~ for improved debuggability and introspectability]]


;; [[file:config.org::*Elpaca-Wait № 1: finish processing current queue][Elpaca-Wait № 1: finish processing current queue:1]]
(elpaca-wait)
;; Elpaca-Wait № 1: finish processing current queue:1 ends here

;; Configure ~use-package~ for improved debuggability and introspectability


;; [[file:config.org::*Configure ~use-package~ for improved debuggability and introspectability][Configure ~use-package~ for improved debuggability and introspectability:1]]
(setopt use-package-expand-minimally nil)
(when (bound-and-true-p init-file-debug)
  (require 'use-package)
  (setopt use-package-expand-minimally nil)
  (setopt use-package-verbose t)
  (setopt use-package-compute-statistics t))
;; Configure ~use-package~ for improved debuggability and introspectability:1 ends here

;; Install ~blackout~ for adjusting modeline indicators :modeline:

;; - Keyword :: =:blackout=


;; [[file:config.org::*Install ~blackout~ for adjusting modeline indicators][Install ~blackout~ for adjusting modeline indicators:1]]
(elpaca blackout
  (require 'blackout))
;; Install ~blackout~ for adjusting modeline indicators:1 ends here

;; Elpaca-Wait № 2: finish processing current queue :wait:

;; - Reason :: Continuing otherwise will result in race conditions where the newly-installed
;; ~use-package~ keywords may or may not be available, resulting in sporadic
;; initialization errors.


;; [[file:config.org::*Elpaca-Wait № 2: finish processing current queue][Elpaca-Wait № 2: finish processing current queue:1]]
(elpaca-wait)
;; Elpaca-Wait № 2: finish processing current queue:1 ends here

;; ~gcmh~: manage running garbage collection on idle :package:perf:

;; - Website :: <https://akrl.sdf.org/>
;; - Code :: <https://gitlab.com/koral/gcmh>

;; During normal use, the GC threshold will be set to a high value.
;; When idle, GC will be triggered with a low threshold.


;; [[file:config.org::*~gcmh~: manage running garbage collection on idle][~gcmh~: manage running garbage collection on idle:1]]
(package! gcmh
  (blackout 'gcmh-mode)
  (add-hook 'ceamx-emacs-startup-hook #'gcmh-mode))
;; ~gcmh~: manage running garbage collection on idle:1 ends here

;; Install utility libraries :package:


;; [[file:config.org::*Install utility libraries][Install utility libraries:1]]
;; FIXME: remove or alias (`##' is very difficult to search for)
(use-package llama) ;  `##' lambda shorthand =>
                                        ;  <https://git.sr.ht/~tarsius/llama>

(use-package f)
;; Install utility libraries:1 ends here

;; TODO Miscellaneous things that should go somewhere else


;; [[file:config.org::*Miscellaneous things that should go somewhere else][Miscellaneous things that should go somewhere else:1]]
;; Increase number of messages saved in log.
(setq message-log-max 10000)

;; Unbind `suspend-frame'.
(global-unset-key (kbd "C-x C-z"))

;; "A second, case-insensitive pass over `auto-mode-alist' is time wasted."
(setopt auto-mode-case-fold nil)

;; Prevent Emacs from pinging domain names unexpectedly.
(setopt ffap-machine-p-known 'reject)
;; Miscellaneous things that should go somewhere else:1 ends here

;; Load Features


;; [[file:config.org::*Load Features][Load Features:1]]
(require 'init-env)
(require 'init-input-methods)

;; Site-specific configuration, to be ignored by version control.
(require 'site-config (file-name-concat user-emacs-directory "site-config") t)

(require 'init-secrets)
;; Load Features:1 ends here

;; [[file:config.org::*Load Features][Load Features:2]]
;;;; Displays + Appearance

;; [[file:config.org::*Load Features][Load Features:4]]
;;;; Displays + Appearance
;; Load Features:4 ends here

;; [[file:config.org::*Load Features][Load Features:5]]
;; Load configuration settings for conditional loading.
(require 'config-ui)

(require 'init-ui)

(when (display-graphic-p)
  (require 'init-ui-graphical))
;; Load Features:5 ends here

;; [[file:config.org::*Load Features][Load Features:6]]
;;;; Dashboard

(require 'init-dashboard)
;; Load Features:6 ends here

;; [[file:config.org::*Load Features][Load Features:7]]
;;;; Keyboard support

(require 'init-keys)
;; Load Features:7 ends here

;; [[file:config.org::*Load Features][Load Features:8]]
;;;; Windows

(require 'init-window)
(require 'init-buffer)

;; FIXME: load earlier / in another section
(require 'init-history)
;; Load Features:8 ends here

;; [[file:config.org::*Load Features][Load Features:9]]
;;;; Text Expansion

(require 'init-abbrevs)
;; Load Features:9 ends here

;; [[file:config.org::*Load Features][Load Features:10]]
;;;; Completions and Selections

(require 'init-search)
(require 'init-completion)
;; Load Features:10 ends here

;; [[file:config.org::*Load Features][Load Features:11]]
;;;; Help

(require 'init-help)
;; Load Features:11 ends here

;; [[file:config.org::*Load Features][Load Features:12]]
;;;; Actions

(require 'init-embark)

;; Projects / Files
(require 'init-project)
(require 'init-vcs)
(require 'init-files)
(require 'init-dired)
;; Load Features:12 ends here

;; [[file:config.org::*Load Features][Load Features:13]]
;;;; Workspaces + activities + contexts

(require 'init-workspace)
;; Load Features:13 ends here

;; [[file:config.org::*Load Features][Load Features:14]]
;;;; Editing

(require 'init-editor)
(require 'init-writing)
(require 'init-templates)
;; Load Features:14 ends here

;; [[file:config.org::*Load Features][Load Features:15]]
;;;; Outlines & Memex

(require 'init-notes)
(require 'init-outline)
(require 'init-org)
;; Load Features:15 ends here

;; [[file:config.org::*Load Features][Load Features:16]]
;;;; Linting

(require 'init-flymake)
(require 'init-flycheck)
;; Load Features:16 ends here

;; [[file:config.org::*Load Features][Load Features:17]]
;;;; Tree-Sitter

(require 'init-treesitter)
;; Load Features:17 ends here

;; [[file:config.org::*Load Features][Load Features:18]]
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
;; Load Features:18 ends here

;; [[file:config.org::*Load Features][Load Features:19]]
;;;; Miscellaneous

(require 'init-tools)

(require 'init-term)
(require 'init-news)
(require 'init-eww)
(require 'init-printing)

(require 'init-fun)

(require 'init-controls)
;; Load Features:19 ends here

;; TODO ~hippie-expand~


;; [[file:config.org::*~hippie-expand~][~hippie-expand~:1]]
(setopt hippie-expand-verbose t
        hippie-expand-dabbrev-skip-space t)
;; ~hippie-expand~:1 ends here

;; [[file:config.org::*~hippie-expand~][~hippie-expand~:2]]
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
;; ~hippie-expand~:2 ends here

;; The Keybindings of Uncertainty :keybinds:


;; [[file:config.org::*The Keybindings of Uncertainty][The Keybindings of Uncertainty:1]]
(define-keymap :keymap ceamx-session-map
  "q" #'save-buffers-kill-emacs
  "Q" #'kill-emacs)
;; The Keybindings of Uncertainty:1 ends here

;; Start the Emacs server process if not already running


;; [[file:config.org::*Start the Emacs server process if not already running][Start the Emacs server process if not already running:1]]
(defun ceamx/maybe-start-server ()
  "Allow this Emacs process to act as server process if not already running."
  (require 'server)
  (unless (and (fboundp 'server-running-p)
               (server-running-p))
    (server-start)))
;; Start the Emacs server process if not already running:1 ends here

;; [[file:config.org::*Start the Emacs server process if not already running][Start the Emacs server process if not already running:2]]
(add-hook 'ceamx-emacs-startup-hook #'ceamx/maybe-start-server)
;; Start the Emacs server process if not already running:2 ends here

;; macOS: Restart Yabai after init

;; Otherwise, =yabai= will not "see" the Emacs GUI window.


;; [[file:config.org::*macOS: Restart Yabai after init][macOS: Restart Yabai after init:1]]
(when (and (display-graphic-p) +sys-mac-p)
  (def-hook! ceamx-after-init-restart-yabai-h ()
    'ceamx-after-init-hook
    "Restart the yabai service after init."
    (after! exec-path-from-shell
      (async-shell-command "yabai --restart-service"))))
;; macOS: Restart Yabai after init:1 ends here

;; Optionally load the ~custom-file~


;; [[file:config.org::*Optionally load the ~custom-file~][Optionally load the ~custom-file~:1]]
(defun ceamx/load-custom-file ()
  "Load the user `custom-file'."
  (interactive)
  (when (file-exists-p custom-file)
    (load custom-file 'noerror)))
;; Optionally load the ~custom-file~:1 ends here

;; [[file:config.org::*Optionally load the ~custom-file~][Optionally load the ~custom-file~:2]]
(add-hook 'ceamx-after-init-hook #'ceamx/load-custom-file)
;; Optionally load the ~custom-file~:2 ends here

;; Load the chaos file


;; [[file:config.org::*Load the chaos file][Load the chaos file:1]]
(add-hook 'ceamx-after-init-hook
          (lambda ()
            (load (locate-user-emacs-file "chaos.el") t)))
;; Load the chaos file:1 ends here

(provide 'init)
;;; init.el ends here
