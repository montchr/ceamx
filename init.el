(defvar ceamx-foo-flag t)
(defvar ceamx-is-foo-enabled t)
(defun ceamx-foo-p ()
  ;; sketchy logic (don't do this)
  (or ceamx-foo-flag ceamx-is-foo-enabled))



(require 'f)
(require 'llama)

(require 'ob-tangle)

(defun ceamx-list-tangled-init-files ()
  "List all tangled files in `user-emacs-directory'.
Note that this is a crude approximation reflective of our
expectations but not necessarily files that were truly tangled.

The assumptions are as follows:

-- All Emacs Lisp files within the \"lisp\" subdirectory
-- early-init.el
-- init.el

If there are any Emacs Lisp files within the \"lisp\"
subdirectory that have not been created as a result of tangling,
they will also be included in the result."
  (let ((dir user-emacs-directory))
    (append
     (mapcar (##f-join dir %) '("early-init.el" "init.el"))
     (f-files (f-join dir "lisp") (##f-ext-p % "el") t))))

(defun ceamx/purge-tangled-init-files ()
  "Delete all tangled init files according to `ceamx-list-tangled-init-files'."
  (interactive)
  (dolist (file (ceamx-list-tangled-init-files))
    (f-delete file)))

(defconst ceamx-literate-config-file (locate-user-emacs-file "config.org"))

(defun ceamx/tangle-fresh (&optional src-file)
  "Purge all existing tangled init files and re-tangle.
When SRC-FILE is non-nil, it will be used as the source file to
be tangled by `org-babel-tangle-file'.  Otherwise, if SRC-FILE is
nil, the value of `ceamx-literate-config-file' will be the
default source file."
  (interactive)
  (ceamx/purge-tangled-init-files)
  (org-babel-tangle-file
   (or src-file ceamx-literate-config-file)))
(ceamx/tangle-fresh)

(require 'f)
(require 'llama)

(require 'ob-tangle)

(defun ceamx-list-tangled-init-files ()
  "List all tangled files in `user-emacs-directory'.
Note that this is a crude approximation reflective of our
expectations but not necessarily files that were truly tangled.

The assumptions are as follows:

-- All Emacs Lisp files within the \"lisp\" subdirectory
-- early-init.el
-- init.el

If there are any Emacs Lisp files within the \"lisp\"
subdirectory that have not been created as a result of tangling,
they will also be included in the result."
  (let ((dir user-emacs-directory))
    (append
     (mapcar (##f-join dir %) '("early-init.el" "init.el"))
     (f-files (f-join dir "lisp") (##f-ext-p % "el") t))))

(defun ceamx/purge-tangled-init-files ()
  "Delete all tangled init files according to `ceamx-list-tangled-init-files'."
  (interactive)
  (dolist (file (ceamx-list-tangled-init-files))
    (f-delete file)))

(defconst ceamx-literate-config-file (locate-user-emacs-file "config.org"))

(defun ceamx/tangle-fresh (&optional src-file)
  "Purge all existing tangled init files and re-tangle.
When SRC-FILE is non-nil, it will be used as the source file to
be tangled by `org-babel-tangle-file'.  Otherwise, if SRC-FILE is
nil, the value of `ceamx-literate-config-file' will be the
default source file."
  (interactive)
  (ceamx/purge-tangled-init-files)
  (org-babel-tangle-file
   (or src-file ceamx-literate-config-file)))

;;; init.el --- Ceamx  -*- lexical-binding: t; no-byte-compile: t; -*-

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

(add-hook 'ceamx-after-init-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract (current-time) before-init-time)))
                     gcs-done)))

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

(defconst ceamx-cheatsheets-dir
  (file-name-as-directory
   (concat ceamx-home-dir "Documents/cheatsheets"))
  "Absolute path to the directory containing user cheatsheets.")

(require 'ceamx-paths)
(require 'ceamx-lib)

(setopt ediff-window-setup-function #'ediff-setup-windows-plain)

;; Version control support is essential as soon as possible.
(require 'vc)

(setopt vc-follow-symlinks t)

;; No need for all that other nonsense.
(setopt vc-handled-backends '(Git))

;; NOTE: According to the documentation for ~diff-hl~, the diff algorithm
;; cannot be determined based on the user's global git config =diff.algorithm=
;; setting. The website source they linked to has disappeared with no archived
;; page available. So I have not verified this for certain.
(setopt vc-git-diff-switches '("--histogram"))

(package! diff-hl
  (add-hook 'ceamx-after-init-hook #'global-diff-hl-mode)

  ;; Display indicators in margins instead of fringes.
  ;; This will work in terminal sessions and also avoid the fringe conflict with
  ;; other indicators like Flycheck errors.
  (add-hook 'ceamx-after-init-hook #'diff-hl-margin-mode)

  ;; Support mouse click on indicator to show hunk.
  (when (display-graphic-p)
    (add-hook 'ceamx-after-init-hook #'diff-hl-show-hunk-mouse-mode)))

;; Committing changes using a package other than `vc' requires integration.
;; <https://github.com/dgutov/diff-hl#integration>
(after! (diff-hl magit)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

;; Enable `dired' integration.
(after! (diff-hl dired)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode))

(package! git-commit
  (require 'git-commit)
  (add-hook 'ceamx-after-init-hook #'global-git-commit-mode))

(after! (git-commit evil)
  (declare-function evil-insert-state "evil")
  (add-hook 'git-commit-mode-hook #'evil-insert-state))

(after! (git-commit meow)
  (declare-function meow-insert-mode "meow")
  (add-hook 'git-commit-mode-hook #'meow-insert-mode))

(package! git-modes)

(package! git-timemachine
  (keymap-global-set "C-x v t" #'git-timemachine))

(after! git-timemachine
  ;; XXX: broken, see `ceamx/git-timemachine-dispatch'
  ;; (add-hook 'git-timemachine-mode-hook #'ceamx/git-timemachine-dispatch)

  (define-keymap :keymap git-timemachine-mode-map
    "M-p" #'git-timemachine-show-previous-revision
    "M-n" #'git-timemachine-show-next-revision
    "M-b" #'git-timemachine-blame
    "M-c" #'git-timemachine-show-commit)

  ;; FIXME: like `ceamx/window-dispatch', this breaks because the commands
  ;; should be run in the original buffer/window
  (transient-define-prefix ceamx/git-timemachine-dispatch ()
    "Transient menu for `git-timemachine-mode'."
    ;; :transient-suffix 'transient--do-stack
    [["Navigation"
      ("p" "previous revision" git-timemachine-show-previous-revision :transient t)
      ("n" "next revision" git-timemachine-show-next-revision :transient t)]
     ["Display"
      ("b" "blame" git-timemachine-blame)
      ("c" "commit" git-timemachine-show-commit )]
     [""
      ("q" "quit" git-timemachine-quit :transient nil)]])

  (declare-function git-timemachine--show-minibuffer-details "git-timemachine")

  ;; via <https://github.com/doomemacs/doomemacs/blob/07fca786154551f90f36535bfb21f8ca4abd5027/modules/emacs/vc/config.el#L76C1-L90C47>
  (def-advice! +git-timemachine--details-in-header-line-a (revision)
    :override #'git-timemachine--show-minibuffer-details
    "Show REVISION details in the header-line instead of the minibuffer."
    (let* ((date-relative (nth 3 revision))
           (date-full (nth 4 revision))
           (author (if git-timemachine-show-author (concat (nth 6 revision) ": ") ""))
           (sha-or-subject (if (eq git-timemachine-minibuffer-detail 'commit) (car revision) (nth 5 revision))))
      (setq header-line-format
            (format "%s%s [%s (%s)]"
                    (propertize author 'face 'git-timemachine-minibuffer-author-face)
                    (propertize sha-or-subject 'face 'git-timemachine-minibuffer-detail-face)
                    date-full date-relative)))))

(package! browse-at-remote
  (keymap-set vc-prefix-map "o" #'browse-at-remote))

(package! magit)

(with-eval-after-load 'magit
  (defvar magit-mode-map)
  (defvar magit-status-mode-map)

  (declare-function magit-discard "magit-apply")
  (declare-function magit-dispatch "magit")
  (declare-function magit-display-buffer-fullframe-status-v1 "magit-mode")
  (declare-function magit-file-dispatch "magit-files")
  (declare-function magit-restore-window-configuration "magit-mode")
  (declare-function magit-revert "magit-sequence")
  (declare-function magit-status "magit-status")

  (setopt magit-diff-refine-hunk t)     ; show granular diffs in selected hunk
  (setopt magit-save-repository-buffers nil) ; avoid side-effects (e.g. auto-format)
  ;; (setopt magit-revision-insert-related-refs nil) ; parent/related refs: rarely useful
  (setopt magit-process-finish-apply-ansi-colors t) ; render ANSI colors in process output

  (setopt magit-bury-buffer-function #'magit-restore-window-configuration)
  ;; <https://magit.vc/manual/magit/Switching-Buffers.html#index-magit_002ddisplay_002dbuffer_002dfullframe_002dstatus_002dv1>
  (setopt magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(after! magit
  (remove-hook 'magit-section-movement-hook 'magit-hunk-set-window-start)
  (add-hook 'magit-section-movement-hook #'magit-section-set-window-start))

(package! magit-todos
  (after! magit
    (magit-todos-mode 1)))

(setopt magit-process-popup-time 3)

(package! forge
  (after! magit
    (require 'forge)))

git config forge.remote 'github'

pass insert api.github.com/montchr^forge

(define-keymap :keymap (current-global-map)
  "C-c g"    #'magit-dispatch
  "C-c G"    #'magit-file-dispatch

  "C-x g"    #'magit-status
  "C-x M-g"  #'magit-dispatch)

(after! magit
  (define-keymap :keymap magit-status-mode-map
    "_" #'magit-revert
    ;; "V" nil
    "x" #'magit-discard)

  (transient-append-suffix 'magit-commit "-n"
    '("-S" "Disable GPG signing" "--no-gpg"))

  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))

  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash")))

(defconst ceamx-projects-dir
  (file-name-as-directory
   (or (getenv "XDG_PROJECTS_DIR")
       (concat ceamx-home-dir "Developer")))
  "The root directory for projects.")
