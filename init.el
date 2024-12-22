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


(require 'cl-lib)

;; Core variables
(require 'ceamx-paths)
(require 'ceamx-keymaps)

;; Core functions and macros
(require 'ceamx-lib)

;; Configure default identity


(setq-default user-full-name "Chris Montgomery"
              user-mail-address "chmont@protonmail.com")

;; Profiling

;; - source :: <https://github.com/progfolio/.emacs.d/blob/ed159dc6076664ad9976949d8cb3af8e86fe39d1/init.org#profiling>


(add-hook 'ceamx-after-init-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract (current-time) before-init-time)))
                     gcs-done)))

;; Add the =site-lisp= directory to ~load-path~


(add-to-list 'load-path ceamx-site-lisp-dir)
(prependq! load-path (ceamx-subdirs ceamx-site-lisp-dir))

;; Initialize the =ceamx= user options


(defgroup ceamx nil
  "User-configurable options for Ceamx."
  ;; TODO: is this group appropriate?
  :group 'file)

;; The user option to determine whether to load ~custom-file~


(defcustom ceamx-load-custom-file nil
  "Whether to load the user `custom-file' (custom.el)."
  :group 'ceamx
  :type '(boolean))

;; =site-lisp/on=: Define additional Emacs event hooks


(require 'on)

;; Set the Elpaca installer version


(defvar elpaca-installer-version 0.8)

;; Show the Elpaca to its house


(defvar elpaca-directory (expand-file-name "elpaca/" ceamx-packages-dir))

;; Summon the Elpaca

;; The installation code only needs to be changed when the Elpaca warns about an
;; installer version mismatch.

;; This should be copied verbatim from the Elpaca documentation, sans the
;; definitions for ~elpaca-installer-version~ and ~elpaca-directory~.


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

;; Bind some commonly-used package management commands :keybinds:


(define-keymap :keymap ceamx-packages-map
  "f" #'elpaca-fetch-all
  "m" #'elpaca-merge-all
  "t" #'elpaca-try)

(keymap-set ceamx-session-map "p" '("Packages" . ceamx-packages-map))

;; TODO Move this global binding somewhere else... but where? :keybinds:


(keymap-global-set "C-c q" ceamx-session-map)

;; Run our custom init and startup hooks on ~elpaca-after-init-hook~


(add-hook 'elpaca-after-init-hook #'ceamx-after-init-hook)
(add-hook 'elpaca-after-init-hook #'ceamx-emacs-startup-hook)

;; Pretend file-visiting-buffers in the package directory are read-only


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


(require 'ceamx-paths)

;; These must be set prior to package load.
(setq no-littering-etc-directory ceamx-etc-dir)
(setq no-littering-var-directory ceamx-var-dir)

(elpaca no-littering
  (require 'no-littering))

;; Install the latest version of ~seq~ builtin library, carefully

;; ~magit~ requires a more recent version of ~seq~ than the version included in
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

;; Install the latest version of the builtin ~jsonrpc~ library

;; Required by (and originally extracted from) ~eglot~.


(elpaca jsonrpc
  (require 'jsonrpc))

;; Install the latest version of the ~eldoc~ builtin library, carefully

;; Required by ~eglot~.

;; ~eldoc~ requires a delicate workaround to avoid catastrophy
;; <https://github.com/progfolio/elpaca/issues/236#issuecomment-1879838229>



(unless after-init-time
  (unload-feature 'eldoc t)
  (setq custom-delayed-init-variables '())
  (defvar global-eldoc-mode nil))

(elpaca eldoc
  (require 'eldoc)
  (global-eldoc-mode))

;; Install the latest version of the builtin ~eglot~ package


(unless after-init-time
  (when (featurep 'eglot)
    (unload-feature 'eglot)))

(elpaca eglot)

;; Install the latest version of the builtin ~flymake~ package


(unless after-init-time
  (when (featurep 'flymake)
    (unload-feature 'flymake)))

(elpaca flymake)

;; Install the latest version of Org-Mode


(unless after-init-time
  (when (featurep 'org)
    (unload-feature 'org)))

(elpaca (org :autoloads "org-loaddefs.el"))

;; Install the latest version of Use-Package


(elpaca use-package)

;; Integrate Elpaca and Use-Package


(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; Use-Package: Ensure package installation by default

;; Equivalent to manually specifying =:ensure t= in each ~use-package~ expression.


(setopt use-package-always-ensure t)

;; Elpaca-Wait № 1: finish processing current queue :wait:

;; Reason:

;; - Continuing otherwise will result in race conditions on the definition of storage paths
;; - ~use-package~ must be loaded for byte-compilation checks in [[*Configure ~use-package~ for improved debuggability and introspectability]]


(elpaca-wait)

;; Configure ~use-package~ for improved debuggability and introspectability


(setopt use-package-expand-minimally nil)
(when (bound-and-true-p init-file-debug)
  (require 'use-package)
  (setopt use-package-expand-minimally nil)
  (setopt use-package-verbose t)
  (setopt use-package-compute-statistics t))

;; Install ~blackout~ for adjusting modeline indicators :modeline:

;; - Keyword :: =:blackout=


(elpaca blackout
  (require 'blackout))

;; Elpaca-Wait № 2: finish processing current queue :wait:

;; - Reason :: Continuing otherwise will result in race conditions where the newly-installed
;; ~use-package~ keywords may or may not be available, resulting in sporadic
;; initialization errors.


(elpaca-wait)

;; ~gcmh~: manage running garbage collection on idle :package:perf:

;; - Website :: <https://akrl.sdf.org/>
;; - Code :: <https://gitlab.com/koral/gcmh>

;; During normal use, the GC threshold will be set to a high value.
;; When idle, GC will be triggered with a low threshold.


(package! gcmh
  (blackout 'gcmh-mode)
  (add-hook 'ceamx-emacs-startup-hook #'gcmh-mode))

;; Install utility libraries :package:


;; FIXME: remove or alias (`##' is very difficult to search for)
(use-package llama) ;  `##' lambda shorthand =>
                                        ;  <https://git.sr.ht/~tarsius/llama>

(use-package f)

;; TODO Miscellaneous things that should go somewhere else


;; Increase number of messages saved in log.
(setq message-log-max 10000)

;; Unbind `suspend-frame'.
(global-unset-key (kbd "C-x C-z"))

;; "A second, case-insensitive pass over `auto-mode-alist' is time wasted."
(setopt auto-mode-case-fold nil)

;; Prevent Emacs from pinging domain names unexpectedly.
(setopt ffap-machine-p-known 'reject)

;; Disable unnecessary OS-specific command-line options :macos:


(unless (ceamx-host-macos-p)
  (setq command-line-ns-option-alist nil))

(unless (ceamx-host-gnu-linux-p)
  (setq command-line-x-option-alist nil))

;; ~exec-path-from-shell~: Inherit environment variables from variable environments :package:


(package! exec-path-from-shell
  (require 'exec-path-from-shell)
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH" "LSP_USE_PLISTS"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; ~inheritenv~: Make temporary buffers inherit buffer-local environment variables :package:

;; - website :: <https://github.com/purcell/inheritenv>


(package! inheritenv
  (with-eval-after-load 'exec-path-from-shell
    (require 'inheritenv)))

;; ~with-editor~: Ensure shell/term modes use session as =$EDITOR= :package:


(package! with-editor
  (keymap-global-set "<remap> <async-shell-command>"
                     #'with-editor-async-shell-command)
  (keymap-global-set "<remap> <shell-command>"
                     #'with-editor-shell-command)

  (add-hook 'shell-mode-hook #'with-editor-export-editor)
  (add-hook 'eshell-mode-hook #'with-editor-export-editor)
  (add-hook 'term-exec-hook #'with-editor-export-editor)

  ;; Make sure that `eat' does not break `magit-commit'.
  ;; <https://codeberg.org/akib/emacs-eat/issues/55#issuecomment-871388>
  (with-eval-after-load 'eat
    (add-hook 'eat-mode-hook #'shell-command-with-editor-mode)))

;; ~envrc~: Direnv integration :package:

;; - src :: <https://github.com/purcell/envrc>
;; - upstream :: <https://github.com/direnv/direnv>

;; Q: How does this differ from `direnv.el`?

;; <https://github.com/wbolster/emacs-direnv> repeatedly changes the global
;; Emacs environment, based on tracking what buffer you're working on.

;; Instead, `envrc.el` simply sets and stores the right environment in each
;; buffer, as a buffer-local variable.


(package! envrc
  (with-eval-after-load 'exec-path-from-shell
    (envrc-global-mode)))

;; Elpaca-Wait № 3: ~exec-path-from-shell~ :wait:


(elpaca-wait)

;; TRAMP Support


(setopt tramp-default-method "ssh")
(setopt tramp-default-remote-shell "/bin/bash")
(setopt tramp-connection-timeout (* 60 10))
;; Do not auto-save remote files. Note the reversed logic.
(setopt remote-file-name-inhibit-auto-save t)                 ; Emacs 30
(setopt remote-file-name-inhibit-auto-save-visited t)
;; Avoid expensive operations on remote files.
(setopt remote-file-name-inhibit-delete-by-moving-to-trash t) ; Emacs 30

(after! tramp
  (dolist (path '("~/.local/bin"
                  "~/.nix-profile/bin"
                  "~/.local/state/nix/profiles/profile/bin/"
                  "/nix/var/nix/profiles/default/bin"
                  "/run/current-system/sw/bin"))
    (add-to-list 'tramp-remote-path path)))

;; Terminal/TTY Support


(autoload 'mwheel-install "mwheel")

(defun ceamx/console-frame-setup ()
  (xterm-mouse-mode 1)
  (mwheel-install))

;; Make the mouse wheel scroll.
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))

;; (add-hook 'after-make-console-frame-hooks 'ceamx/console-frame-setup)

;; Input languages


(set-language-environment "UTF-8")

;; `set-language-environment' also presumptively sets `default-input-method'.
(setopt default-input-method nil)



;; Disable bidirectional text scanning, because I don't need it:


(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Mouse input


(setopt mouse-yank-at-point t)



;; Avoid collision of mouse with point:


(mouse-avoidance-mode 'exile)

;; Disable graphical window system dialog boxes


(setopt use-file-dialog nil)
(setopt use-dialog-box nil)

;; Load site-specific configuration, to be ignored by version control


(require 'site-config (file-name-concat user-emacs-directory "site-config") t)

;; Secrets :: =secrets= :secrets:
;; :PROPERTIES:
;; :header-args: :tangle init.el
;; :END:

;; - source :: <https://github.com/jwiegley/dot-emacs/blob/9d595c427136e2709dee33271db1a658493265bd/init.org#auth-source-pass>


(require 'epg)
(require 'auth-source)
(require 'auth-source-pass)

;; Ensure secrets and auth credentials are not stored in plaintext

;; It's best to list only a single file here to avoid confusion about where
;; secrets might be stored:


(setopt auth-sources (list "~/.authinfo.gpg"))

;; Configure secrets lookup with ~auth-source~ and the Unix password store


(use-feature! auth-source
  :demand t)

;; TODO: provide explanation as to why these functions are named like so -- they just magically work..?
(use-feature! auth-source-pass
  :demand t

  :preface
  (defvar auth-source-pass--cache (make-hash-table :test #'equal))

  (defun auth-source-pass--reset-cache ()
    (setq auth-source-pass--cache (make-hash-table :test #'equal)))

  (defun auth-source-pass--read-entry (entry)
    "Return a string with the file content of ENTRY."
    (run-at-time 45 nil #'auth-source-pass--reset-cache)
    (let ((cached (gethash entry auth-source-pass--cache)))
      (or cached
          (puthash
           entry
           (with-temp-buffer
             (insert-file-contents (expand-file-name
                                    (format "%s.gpg" entry)
                                    (getenv "PASSWORD_STORE_DIR")))
             (buffer-substring-no-properties (point-min) (point-max)))
           auth-source-pass--cache))))

  (defun ceamx-auth-source-pass-list-items ()
    "Return a list of all password store items."
    (let ((store-dir (getenv "PASSWORD_STORE_DIR")))
      (mapcar
       (lambda (file)
         (file-name-sans-extension (file-relative-name file store-dir)))
       (directory-files-recursively store-dir "\.gpg$"))))

  :config
  (auth-source-pass-enable))

;; Use Emacs for =pinentry=


(use-feature! epg
  :defer 2
  :config
  (setopt epg-pinentry-mode 'loopback))

;; Define helper function to lookup a password for a target host

;; - source :: <https://github.com/jwiegley/dot-emacs/blob/9d595c427136e2709dee33271db1a658493265bd/init.org#lookup-a-password-using-auth-source>


(defun ceamx-lookup-password (host user port)
  (require 'auth-source)
  (require 'auth-source-pass)
  (let ((auth (auth-source-search :host host :user user :port port)))
    (if auth
      (let ((secretf (plist-get (car auth) :secret)))
        (if secretf
          (funcall secretf)
          (error "Auth entry for %s@%s:%s has no secret!"
            user host port)))
      (error "No auth entry found for %s@%s:%s" user host port))))

;; =init.el=: Load Features
;; :PROPERTIES:
;; :header-args: :tangle init.el
;; :END:



;;;; Displays + Appearance

;; Load configuration settings for conditional loading.
(require 'config-ui)

(require 'init-ui)

(when (display-graphic-p)
  (require 'init-ui-graphical))

;;;; Dashboard

(require 'init-dashboard)

;;;; Keyboard support

(require 'init-keys)

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

(require 'init-notes)
(require 'init-outline)
(require 'init-org)

;;;; Linting

(require 'init-flymake)
(require 'init-flycheck)

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

;;;; Miscellaneous

(require 'init-tools)

(require 'init-term)
(require 'init-news)
(require 'init-eww)
(require 'init-printing)

(require 'init-fun)

(require 'init-controls)

;; TODO ~hippie-expand~


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

;; The Keybindings of Uncertainty :keybinds:


(define-keymap :keymap ceamx-session-map
  "q" #'save-buffers-kill-emacs
  "Q" #'kill-emacs)

;; Start the Emacs server process if not already running


(defun ceamx/maybe-start-server ()
  "Allow this Emacs process to act as server process if not already running."
  (require 'server)
  (unless (and (fboundp 'server-running-p)
               (server-running-p))
    (server-start)))

(add-hook 'ceamx-emacs-startup-hook #'ceamx/maybe-start-server)

;; macOS: Restart Yabai after init

;; Otherwise, =yabai= will not "see" the Emacs GUI window.


(when (and (ceamx-host-macos-p) (display-graphic-p))
  (def-hook! ceamx-after-init-restart-yabai-h ()
    'ceamx-after-init-hook
    "Restart the yabai service after init."
    (after! exec-path-from-shell
      (async-shell-command "yabai --restart-service"))))

;; Optionally load the ~custom-file~


(defun ceamx/load-custom-file ()
  "Load the user `custom-file'."
  (interactive)
  (when (file-exists-p custom-file)
    (load custom-file 'noerror)))

(add-hook 'ceamx-after-init-hook #'ceamx/load-custom-file)

;; Load the chaos file


(add-hook 'ceamx-after-init-hook
          (lambda ()
            (load (locate-user-emacs-file "chaos.el") t)))
