;;; init.el --- Ceamx -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (c) 2022-2025  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;; Code:

;; Requirements


(require 'cl-lib)

(require 'ceamx-paths)
(require 'ceamx-lib)
(require 'ceamx-keymaps)

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

;; Initialize the =ceamx= user options


(defgroup ceamx nil
  "User-configurable options for Ceamx."
  :group 'emacs)

;; The user option to define directory trees whose files should be opened in read-only buffers :config:


(defcustom ceamx-buffer-read-only-dirs-list (list ceamx-packages-dir)
  "List of directories whose files should be opened in read-only buffers."
  :group 'ceamx
  :type '(string))

;; The user option to determine whether to load ~custom-file~


(defcustom ceamx-load-custom-file t
  "Whether to load the user `custom-file' (custom.el)."
  :group 'ceamx
  :type '(boolean))

;; Configure ~custom-file~ location
;; :PROPERTIES:
;; :ID:       59f6ed70-cdb4-45fd-8980-0d57d1aad12e
;; :END:


(setq custom-file (locate-user-emacs-file "custom.el"))

;; Security improvements
;; :PROPERTIES:
;; :ID:       870f1c8f-5998-4960-9c0d-98121a73df16
;; :END:


;; Prevent Emacs from pinging domain names unexpectedly.
(setq ffap-machine-p-known 'reject)

(with-eval-after-load 'gnutls
  (eval-when-compile
    (require 'gnutls))

  ;; Disallow insecure TLS connections.
  (setq gnutls-verify-error t)
  ;; This is an acceptably-modern security expectation.
  (setq gnutls-min-prime-bits 3072))

;; Declare safe local variable directories and values relating to Emacs initialization
;; :PROPERTIES:
;; :ID:       dfe910c3-8b2f-4702-b380-ae8d669b981e
;; :END:


(setopt safe-local-variable-directories
        ;; NOTE: `user-emacs-directory' is intentionally not included
        ;; here because its value can change based on the value of the
        ;; "--init-directory" initialization flag.
        (list (file-name-concat ceamx-config-dir "emacs")
              (file-name-concat ceamx-config-dir "ceamx")))
(setopt safe-local-variable-values
        '((eval load-file "./ceamx-dev-loader.el")
          (eval add-hook 'after-save-hook #'org-gfm-export-to-markdown t t)))

;; Enable/disable some commands that are disabled/enabled by default
;; :PROPERTIES:
;; :ID:       b1ce92a4-139c-4054-9fa4-982d47c720d3
;; :END:


;; Enable these commands
(dolist (cmd '(downcase-region
               list-timers
               narrow-to-page
               narrow-to-region
               upcase-region
               scroll-left
               scroll-right))
  (put cmd 'disabled nil))

;; Disable these commands
(dolist (cmd '( diary
                iconify-frame
                overwrite-mode
                suspend-frame))
  (put cmd 'disabled t))

;; Display the scratch buffer as initial buffer


(setq initial-buffer-choice nil
      initial-major-mode 'lisp-interaction-mode
      inhibit-startup-screen t)
(setq initial-scratch-message
      (format ";; This is `%s'.  Use `%s' to evaluate and print results.\n\n"
              'lisp-interaction-mode
              (propertize
               (substitute-command-keys "\\<lisp-interaction-mode-map>\\[eval-print-last-sexp]")
               'face 'help-key-binding)))

;; =site-lisp/on=: Define additional Emacs event hooks


(require 'on)

;; Elpaca


(defvar elpaca-directory (expand-file-name "elpaca/" ceamx-packages-dir))

;; Avoid aggressive GitHub API rate limiting.
(defvar elpaca-queue-limit 30)



;; Elpaca needs to know about the Nix build date of the current version
;; of Emacs to set ~elpaca-core-date~ correctly.  [[https://github.com/progfolio/elpaca/wiki/Usage-with-Nix#retrieving-the-date-via-file-name][From the wiki]]:


(require 'ceamx-lib)

;; FIXME: only works for emacs-git
;; TODO: this should probably take effect for *any* Nix-built Emacs
;; package, not just on NixOS
;; (when (ceamx-host-nixos-p)
;;   (setq elpaca-core-date (list (ceamx-emacs-nix-build-date))))



;; The installation code only needs to be changed when the Elpaca warns
;; about an installer version mismatch.

;; This should be copied verbatim from the Elpaca documentation, with the
;; definition for ~elpaca-directory~ removed.


(defvar elpaca-installer-version 0.11)
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Add a command to reload a package after update without restarting Emacs

;; + Source :: [[https://github.com/progfolio/elpaca/wiki/Reloading-a-package’s-features-after-updating-a-package][Reloading a package’s features after updating a package · progfolio/elpaca Wiki]]


(defun +elpaca-reload-package (package &optional allp)
  "Reload PACKAGE's features.
If ALLP is non-nil (interactively, with prefix), load all of its
features; otherwise only load ones that were already loaded.

This is useful to reload a package after upgrading it.  Since a
package may provide multiple features, to reload it properly
would require either restarting Emacs or manually unloading and
reloading each loaded feature.  This automates that process.

Note that this unloads all of the package's symbols before
reloading.  Any data stored in those symbols will be lost, so if
the package would normally save that data, e.g. when a mode is
deactivated or when Emacs exits, the user should do so before
using this command."
  (interactive
   (list (let ((elpaca-overriding-prompt "Reload package: "))
           (elpaca--read-queued))
         current-prefix-arg))
  ;; This finds features in the currently installed version of PACKAGE, so if
  ;; it provided other features in an older version, those are not unloaded.
  (when (yes-or-no-p (format "Unload all of %s's symbols and reload its features? " package))
    (let* ((package-name (symbol-name package))
           (package-dir (file-name-directory
                         (locate-file package-name load-path (get-load-suffixes))))
           (package-files (directory-files package-dir 'full (rx ".el" eos)))
           (package-features
            (cl-loop for file in package-files
                     when (with-temp-buffer
                            (insert-file-contents file)
                            (when (re-search-forward (rx bol "(provide" (1+ space)) nil t)
                              (goto-char (match-beginning 0))
                              (cadadr (read (current-buffer)))))
                     collect it)))
      (unless allp
        (setf package-features (seq-intersection package-features features)))
      (dolist (feature package-features)
        (ignore-errors
          ;; Ignore error in case it's not loaded.
          (unload-feature feature 'force)))
      (dolist (feature package-features)
        (require feature))
      (when package-features
        (message "Reloaded: %s" (mapconcat #'symbol-name package-features " "))))))

;; Run the custom init and startup hooks on ~elpaca-after-init-hook~


(add-hook 'elpaca-after-init-hook #'ceamx-after-init-hook)
(add-hook 'elpaca-after-init-hook #'ceamx-emacs-startup-hook)

;; Pretend file-visiting-buffers in the package directory are read-only


(require 'ceamx-simple)

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

;; Install the latest version of Org-Mode


(unless after-init-time
  (when (featurep 'org)
    (unload-feature 'org)))

(elpaca (org :autoloads "org-loaddefs.el"))

;; Install the latest version of ~use-package~


(elpaca use-package)

;; =elpaca-use-package=: integrate ~elpaca~ and ~use-package~


(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; Elpaca-Wait № 1: ~no-littering~ + ~use-package~ :wait:

;; Reason:

;; - Continuing otherwise will result in race conditions on the
;;   definition of storage paths.
;; - ~use-package~ must be loaded for byte-compilation checks


(elpaca-wait)

;; Configure ~use-package~ behavior


(setopt use-package-always-ensure t)
(setopt use-package-expand-minimally t)

(when (bound-and-true-p init-file-debug)
  (require 'use-package)
  (setopt use-package-expand-minimally nil)
  (setopt use-package-verbose t)
  (setopt use-package-compute-statistics t))

;; Improve ~use-package~ completion-at-point availability


(def-advice! ceamx+use-package--bind-handle-sharp-quotes-a (args)
  :filter-args #'use-package-normalize-binder
  "Make `use-package' handle sharp-quoted functions correctly in `:bind'.
Performs a recursive find-and-replace on sharp quotes in the arguments,
because that's the simple solution and the performance overhead is
unimportant since it happens during compilation anyway."
  (ceamx--remove-sharp-quotes args))

;; Install utility libraries


(package! llama
  (require 'llama))
(package! f)
(package! request)
(package! transient
  (require 'transient)
  (after! transient
    (keymap-set transient-map "<escape>" #'transient-quit-one)))
(package! (org-mem :host github :repo "meedstrom/org-mem"))

;; Install the =persist= package from =emacsmirror= because of GNU ELPA server issues


;; (package! (persist :host github :repo "emacsmirror/persist")
;;   (require 'persist))

;; Disable unnecessary OS-specific command-line options :macos:


(unless (ceamx-host-macos-p)
  (setq command-line-ns-option-alist nil))

(unless (ceamx-host-gnu-linux-p)
  (setq command-line-x-option-alist nil))

;; =exec-path-from-shell=: Inherit environment variables from variable environments :package:


(package! exec-path-from-shell
  (require 'exec-path-from-shell)
  (dolist (var '("SSH_AUTH_SOCK"
                 "SSH_AGENT_PID"
                 "GPG_AGENT_INFO"
                 "LANG"
                 "LC_CTYPE"
                 "NIX_SSL_CERT_FILE"
                 "NIX_PATH"
                 "LSP_USE_PLISTS"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; =inheritenv=: Make temporary buffers inherit buffer-local environment variables :package:

;; - website :: <https://github.com/purcell/inheritenv>


(package! inheritenv
  (with-eval-after-load 'exec-path-from-shell
    (require 'inheritenv)))

;; =with-editor=: Ensure shell/term modes use session as =$EDITOR= :package:


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

;; =envrc= :: Direnv integration :package:

;; - src :: <https://github.com/purcell/envrc>
;; - upstream :: <https://github.com/direnv/direnv>

;; Q: How does this differ from `direnv.el`?

;; <https://github.com/wbolster/emacs-direnv> repeatedly changes the global
;; Emacs environment, based on tracking what buffer you're working on.

;; Instead, `envrc.el` simply sets and stores the right environment in each
;; buffer, as a buffer-local variable.


(package! envrc
  (after! exec-path-from-shell
    (envrc-global-mode))
  (after! popper
    (add-to-list 'popper-reference-buffers "\\*envrc\\*")))

;; Elpaca-Wait № 3 :wait:


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

;; Input languages


(set-language-environment "UTF-8")

;; `set-language-environment' also presumptively sets `default-input-method'.
(setopt default-input-method nil)

;; Disable bidirectional text scanning
;; (setq-default bidi-display-reordering 'left-to-right)
;; (setq-default bidi-paragraph-direction 'left-to-right)
;; (setq bidi-inhibit-bpa t)

;; Mouse support


(setopt mouse-yank-at-point t)

;; Avoid collision of mouse with point
(mouse-avoidance-mode 'exile)



;; Support scrolling with the mouse wheel or trackpad gestures within
;; non-graphical frames.  Mouse support is available by default in
;; graphical frames.


(unless (display-graphic-p)

  ;; Basic mouse support e.g. click and drag
  (xterm-mouse-mode 1)

  ;; By default, `scroll-down' and `scroll-up' scroll by a huge amount.
  (eval-and-compile
    (defun ceamx/scroll-down ()
      "Scroll down one line."
      (interactive)
      (scroll-down 1))

    (defun ceamx/scroll-up ()
      "Scroll up one line."
      (interactive)
      (scroll-up 1)))

  (global-set-key [mouse-4] #'ceamx/scroll-down)
  (global-set-key [mouse-5] #'ceamx/scroll-up))

;; Load site-specific configuration, to be ignored by version control


(require 'site-config (file-name-concat user-emacs-directory "site-config") t)

;; =init.el= :: Miscellaneous Variables
;; :PROPERTIES:
;; :header-args: :tangle init.el
;; :END:


(defconst ceamx-text-mode-derived-prog-modes-list
  '(nxml-mode sgml-mode toml-ts-mode yaml-mode)
  "Programming modes who are sadly derived from `text-mode'.")

;; Load Features
;; :PROPERTIES:
;; :header-args: :tangle init.el
;; :VISIBILITY: folded
;; :ID:       4e93b7dc-8c0c-44b5-903b-f86ea342fd61
;; :END:


(require 'ceamx-init-ui)
(require 'ceamx-init-modeline)
(require 'ceamx-init-essentials)
(require 'ceamx-init-completion)
(require 'ceamx-init-search)
(require 'ceamx-init-dired)
(require 'ceamx-init-window)
(require 'ceamx-init-vcs)
(require 'ceamx-init-langs)
(require 'ceamx-init-notes)
(require 'ceamx-init-org)
(require 'ceamx-init-tools)
(require 'ceamx-init-news)
(require 'ceamx-init-eww)
(require 'ceamx-init-printing)
(require 'ceamx-init-fun)
(require 'ceamx-init-flows)

;; =ceamx-focus= :: A custom focus mode to enable/disable other modes
;; :LOGBOOK:
;; - Refiled on [2025-07-31 Thu 20:54]
;; :END:

;; This could potentially be abstracted to a generalized mode-toggling mode
;; factory.  Yep.  Or not.

;; This is loaded late because it depends on all the other stuff being
;; loaded.  There’s probably a safer way to write it.


(use-feature! ceamx-focus
  :bind
  ( :map ceamx-toggle-prefix
    ;; "y" because not quite "zzzzzzzz" because z already bound
    ("y" . ceamx-focus-mode)))

;; Superglobals


(define-keymap :keymap (current-global-map)
  "ESC ESC" #'ceamx/keyboard-quit-dwim

  "C-g" #'ceamx/keyboard-quit-dwim
  "C-h" help-map

  "S-RET" #'crux-smart-open-line)

;; [C-] :: Global Control-Modified


(define-keymap :keymap (current-global-map)
  "C-`" #'popper-toggle
  "C-~" #'popper-cycle
  "C-M-`" #'popper-toggle-type
  "C-^" #'crux-top-join-line
  "C-+" #'expreg-expand
  "C-=" #'expreg-contract
  "C-;" #'avy-goto-char-timer
  ;; "C-'" :: RESERVED: special commands e.g. `avy-org-goto-heading-timer'
  "C-." #'embark-act
  "C-<" #'ceamx-simple/escape-url-dwim

  "C-a" #'mwim-beginning
  "C-e" #'mwim-end

  "C-RET" #'ceamx-simple/new-line-below
  "C-S-RET" #'crux-smart-open-line-above

  "C-M-SPC" #'puni-mark-sexp-at-point
  "C-M-@" #'easy-mark
  "C-M-#" #'consult-register
  "C-M-$" #'jinx-languages

  "C-S-d" #'crux-duplicate-current-line-or-region
  "C-M-S-d" #'crux-duplicate-and-comment-current-line-or-region

  "C-k" #'crux-smart-kill-line

  ;; TODO: redundant with `easy-kill'
  "C-S-w" #'ceamx-simple/copy-line

  "C-S-y" #'ceamx-simple/yank-replace-line-or-region)

(after! prog-mode
  (define-keymap :keymap prog-mode-map
    ;; Move forward out of one sexp level
    "C-M-d" #'up-list))

(after! puni
  (define-keymap :keymap puni-mode-map
    "C-M-f" #'puni-forward-sexp
    "C-M-b" #'puni-backward-sexp
    "C-M-a" #'puni-beginning-of-sexp
    "C-M-e" #'puni-end-of-sexp
    "C-M-[" #'puni-backward-sexp-or-up-list
    "C-M-]" #'puni-forward-sexp-or-up-list))

(after! org
  (define-keymap :keymap org-mode-map
    "C-'" #'avy-org-goto-heading-timer
    "C-\"" #'avy-org-refile-as-child
    "C-M-<return>" #'org-insert-subheading
    "C-M-S-<return>" #'org-insert-todo-subheading

    "C-a" #'org-beginning-of-line
    "C-e" #'org-end-of-line))

;; [C-h] :: Help Map


(define-keymap :keymap (current-global-map)
  "C-h b" #'embark-bindings
  "C-h B" #'describe-bindings
  "C-h c" #'helpful-callable
  "C-h C" #'helpful-command
  "C-h D" #'devdocs-lookup
  "C-h f" #'helpful-function
  "C-h F" #'describe-face
  ;; FIXME: conflict
  ;; "C-h F" #'apropos-function
  "C-h h" #'helpful-at-point
  "C-h i" (cons "[ INFO ]" #'ceamx-info-prefix)
  "C-h i i" #'ceamx/consult-info-dwim
  "C-h i c" #'ceamx/completion-info
  "C-h i e" #'ceamx/emacs-info
  "C-h i o" #'ceamx/org-info
  "C-h I" #'consult-info
  "C-h k" #'helpful-key
  "C-h K" (cons "[ KEYBINDS ]" #'ceamx-help-keybindings-prefix)
  "C-h K b" #'help-find-keybinding
  "C-h K f" #'help-find-function
  "C-h K K" #'helpful-key
  "C-h l" #'find-library
  "C-h L" #'apropos-library
  "C-h m" #'consult-man
  "C-h M" #'describe-mode
  "C-h o" #'helpful-symbol
  "C-h t" #'describe-text-properties
  "C-h U" #'apropos-user-option
  "C-h v" #'helpful-variable
  "C-h V" #'apropos-variable)

;; [C-c] :: Operator Prefix


(define-keymap :keymap (current-global-map)
  "C-c a" #'org-agenda
  "C-c b" (cons "[ BUFFER    ]" #'ceamx-buffer-prefix)
  "C-c c" (cons "[ CAPTURE   ]" #'ceamx-capture-prefix)
  ;; "C-c d"
  "C-c e" (cons "[ EDIT      ]" #'ceamx-structural-editing-prefix)
  "C-c f" (cons "[ FILE      ]" #'ceamx-file-prefix)
  "C-c g" #'magit-file-dispatch
  "C-c G" #'magit-dispatch
  "C-c h" (cons "[ HISTORY   ]" #'ceamx-history-prefix)
  "C-c i" (cons "[ INSERT    ]" #'ceamx-insert-prefix)
  ;; "C-c j"
  "C-c k" #'consult-kmacro
  ;; TODO: disambiguate?
  "C-c K" (cons "[ KRYPTION  ]" #'ceamx-cryption-prefix)
  "C-c l" (cons "[ LANG      ]" #'ceamx-code-prefix)
  ;; TODO: disambiguate f/F
  "C-c l f" (cons "folding..." (define-prefix-command 'ceamx-code-f-prefix))
  "C-c l F" (cons "formatting..." (define-prefix-command 'ceamx-code-F-prefix))
  "C-c m" (cons "[ BOOKMARK  ]" #'ceamx-bookmark-prefix)
  "C-c n" (cons "[ NOTE      ]" #'ceamx-note-prefix)
  "C-c o" (cons "[ LAUNCH    ]" #'ceamx-launch-prefix)
  "C-c p" (cons "[ COMPLETE  ]" #'ceamx-completion-prefix)
  "C-c P" #'completion-at-point
  "C-c q" (cons "[ SESSION   ]" #'ceamx-session-prefix)
  ;; "C-c r"
  ;; "C-c s"
  "C-c t" (cons "[ TOGGLE    ]" #'ceamx-toggle-prefix)
  ;; "C-c u"
  ;; "C-c v"
  "C-c w" (cons "[ WORKSPACE ]" #'ceamx-workspace-prefix)
  "C-c W" (cons "[ WEB       ]" #'ceamx-web-prefix)
  ;; FIXME: something else... (btw this is originally set somewhere else...)
  "C-c x" #'ceamx/macrostep-expand
  "C-c y" (cons "[ SNIPPET   ]" #'ceamx-snippet-prefix)
  "C-c z" (cons "[ FOLD      ]" #'ceamx-fold-prefix)

  "C-c M-x" #'consult-mode-command)

;; [C-c !] :: CHECKS :checkers:flycheck:flymake:

;; Flycheck automatically binds its keymap to this.


(define-keymap :keymap (current-global-map)
  "C-c ! l" #'flymake-show-buffer-diagnostics
  "C-c ! n" #'flymake-goto-next-error
  "C-c ! p" #'flymake-goto-previous-error
  "C-c ! c" #'flymake-show-buffer-diagnostics)

;; [C-c b] :: BUFFER :buffer:


(define-keymap :keymap ceamx-buffer-prefix
  "f" #'crux-cleanup-buffer-or-region

  "M-w" #'crux-kill-buffer-truename)

;; [C-c c] :: CAPTURE :capture:


(define-keymap :keymap ceamx-capture-prefix-map
  ;; TODO: <https://protesilaos.com/emacs/denote#text-h:eb72086e-05be-4ae3-af51-7616999fc7c9>
  "r" #'denote-region)

;; [C-c h] :: HISTORY


(define-keymap :keymap ceamx-history-prefix
  "f" #'consult-recent-file
  "h" #'consult-history)

;; [C-c i] :: INSERT


(define-keymap :keymap ceamx-insert-prefix-map
  "d" #'ceamx-simple/insert-date
  "I" '("icon" . nerd-icons-insert)
  "L" #'spdx-insert-spdx
  "n" #'org-node-insert-link
  "s" #'yas-insert-snippet
  "u" #'uuidgen-4)

(after! org
  (keymap-set org-mode-map "C-c i l" #'org-web-tools-insert-link-for-url))

;; [C-c f] :: FILE


(define-keymap :keymap ceamx-file-prefix
  ;; TODO
  ;; "y" #'+yank-this-file-name

  "c" '("copy..." . crux-copy-file-preserve-attributes)
  "d" '("delete" . ceamx-simple/delete-current-file)
  "f" #'find-file
  "F" #'find-file-other-window
  ;; `crux-rename-file-and-buffer' is another option, but its `vc'
  ;; integration is rather annoying, requiring that changes be committed
  ;; to version control or reverted before renaming the file.  Though
  ;; that does sound sensible on paper...
  ;; "r" '("move..." . ceamx-simple/move-current-file) ; alt.
  ;; "r"
  "r" '("move..." . crux-rename-file-and-buffer)

  "s" #'save-buffer
  "U" #'ceamx-simple/sudo-find-file

  "C-d" '("diff with..." . ceamx-simple/diff-with-file))

;; [C-c l] :: LANG :lsp:


(define-keymap :keymap ceamx-code-prefix
  "j" #'ceamx/dumb-jump-dispatch/body
  "o" nil                               ; RESERVED :: `combobulate-key-prefix'
  )

(after! eglot
  (define-keymap :keymap eglot-mode-map
    "C-c l a" #'eglot-code-actions
    "C-c l l" (define-prefix-command 'ceamx-lang-specific-prefix)
    "C-c l o" #'consult-eglot-symbols
    "C-c l r" #'eglot-rename))

(after! lsp-mode
    (keymap-global-set "C-c l o" #'consult-lsp-symbols)
    ;; Override the default binding for `xref-find-apropos'.
    (keymap-set lsp-mode-map "C-M-." #'consult-lsp-symbols))

(after! csv-mode
  (define-keymap :keymap csv-mode-map
    "C-c l l a" #'csv-align-fields
    "C-c l l u" #'csv-unalign-fields
    "C-c l l s" #'csv-sort-fields
    "C-c l l S" #'csv-sort-numeric-fields
    "C-c l l k" #'csv-kill-fields
    "C-c l l t" #'csv-transpose))

;; [C-c K] :: KRYPTION


(define-keymap :keymap ceamx-cryption-prefix-map
  "d" (cons "decrypt..." (define-prefix-command 'ceamx-cryption-d-prefix))
  "d d" #'epa-decrypt-file
  "d r" #'epa-decrypt-region
  "e" (cons "encrypt..." (define-prefix-command 'ceamx-cryption-e-prefix))
  "e e" #'epa-encrypt-file
  "e r" #'epa-encrypt-region
  "k" #'epa-list-keys)

;; [C-c m] :: BOOKMARK :bookmarks:


(define-keymap :keymap ceamx-bookmark-prefix-map
  "b" #'bookmark-in-project-jump
  "m" #'consult-bookmark
  "n" #'bookmark-in-project-jump-next
  "p" #'bookmark-in-project-jump-previous
  "*" #'bookmark-in-project-toggle)

;; [C-c n] :: NOTE :notes:


(define-keymap :keymap ceamx-note-prefix-map
  "n" #'denote
  "d" #'denote-sort-dired
  "r" #'denote-rename-file
  "R" #'denote-rename-file-using-front-matter
  "s" #'consult-notes)

(after! org
  (define-keymap :keymap org-mode-map
    "C-c n h" #'denote-org-extras-extract-org-subtree
    "C-c n l" #'denote-link
    "C-c n L" #'denote-add-links
    "C-c n b" #'denote-backlinks))

;; [C-c o] :: LAUNCH


(define-keymap :keymap ceamx-launch-prefix
  "a" #'org-agenda
  "b" #'eww
  "f" #'elfeed
  "s" #'scratch-buffer
  "t" #'eat
  "W" #'ceamx/eww-wiki)

;; [C-c p] :: COMPLETE :completion:


;; cf. `cape-prefix-map' for ideas
(define-keymap :keymap ceamx-completion-prefix-map
  "a" #'cape-abbrev
  "d" #'cape-dabbrev
  "e" (cape-capf-interactive #'elisp-completion-at-point)
  "f" #'cape-file
  "o" #'cape-elisp-symbol
  "p" #'completion-at-point
  "w" #'cape-dict)

;; [C-c q] :: SESSION


(define-keymap :keymap ceamx-session-prefix
  "a c" #'cursory-set-preset
  "a d" #'ceamx-ui/dark
  "a f" #'fontaine-set-preset
  "a l" #'ceamx-ui/light
  "a t" #'consult-theme
  "a o" #'olivetti-mode

  "p" (cons "packages..." (define-prefix-command 'ceamx-session-p-prefix))

  "q" #'save-buffers-kill-emacs
  "Q" #'kill-emacs
  "r" #'restart-emacs)

(define-keymap :keymap ceamx-session-p-prefix
  "f" #'elpaca-fetch-all
  "F" #'elpaca-fetch
  "i" #'elpaca-info
  "m" #'elpaca-merge-all
  "p" #'elpaca-pull
  "r" #'+elpaca-reload-package
  "t" #'elpaca-try
  "u" #'elpaca-update)

;; (use-feature! ceamx-ui
;;   :commands (ceamx-ui/dark ceamx-ui/light)
;;   :config
;;   (define-keymap :keymap ceamx-session-prefix
;;     "a d" #'ceamx-ui/dark
;;     "a l" #'ceamx-ui/light))

;; [C-c t] :: TOGGLE


(define-keymap :keymap ceamx-toggle-prefix
  "c" (cons "cycle..." (define-prefix-command 'ceamx-toggle-c-prefix))
  "c c" #'cycle-at-point
  "c s" #'string-inflection-toggle
  "c +" #'shift-number-up
  "c -" #'shift-number-down

  "f" #'flycheck-mode
  "k" #'keycast-mode-line-mode
  "l" #'display-line-numbers-mode
  "M" #'menu-bar-mode
  "o" #'outline-minor-mode
  "s" #'jinx-mode
  "t" #'typo-mode
  "T" #'tab-bar-mode
  "w" #'window-toggle-side-windows
  "W" #'toggle-window-dedicated
  "z" #'logos-focus-mode
  "Z" #'focus-mode)

(after! dired
  (keymap-set dired-mode-map "C-c t p" #'dired-preview-global-mode))

;; [C-c w] :: WORKSPACE :window:workspace:


(define-keymap :keymap ceamx-workspace-prefix
  "w" #'ceamx/window-dispatch

  "a" (cons "[ ACTIVITIES ]" #'ceamx-activities-prefix)

  "a RET" #'activities-switch
  "a a" #'activities-resume
  "a d" #'activities-define
  "a g" #'activities-revert
  "a k" #'activities-kill
  "a n" #'activities-new
  "a s" #'activities-suspend

  "b" #'bufler-workspace-switch-buffer
  "B" #'bufler-workspace-focus-buffer
  "o" #'bufler-workspace-open
  "r" #'bufler-workspace-save)

;; [C-c W] :: WEB :web:


(define-keymap :keymap ceamx-web-prefix-map
  "i" #'org-web-tools-insert-link-for-url
  "O" #'org-web-tools-read-url-as-org)

(after! org-mode
  (keymap-set org-mode-map "C-c w a" #'org-web-tools-archive-attach)
  (keymap-set org-mode-map "C-c w c" #'org-web-tools-convert-links-to-page-entries))

;; [C-c y] :: SNIPPET :snippets:


(define-keymap :keymap ceamx-snippet-prefix
  "n" #'yas-new-snippet
  "v" #'yas-visit-snippet-file
  "y" #'yas-insert-snippet
  "Y" #'tempel-insert)

;; [C-c z] :: FOLD

;; IIRC, Vim uses some =z= stuff for folding too.


(keymap-set ceamx-fold-prefix "z" #'treesit-fold-toggle)

(define-keymap :keymap treesit-fold-mode-map
  "C-c z c" #'treesit-fold-close
  "C-c z C" #'treesit-fold-close-all
  "C-c z o" #'treesit-fold-open
  "C-c z O" #'treesit-fold-open-all
  "C-c z r" #'treesit-fold-open-recursively
  "C-c z t" #'treesit-fold-toggle
  "C-z z z" #'treesit-fold-toggle)

;; [C-x] :: Global X-Prefix


(define-keymap :keymap (current-global-map)
  "C-x =" #'balance-windows
  "C-x +" #'balance-windows-area
  "C-x ]" #'logos-forward-page-dwim
  "C-x [" #'logos-backward-page-dwim
  "C-x <" #'scroll-right
  "C-x >" #'scroll-left
  "C-x SPC" #'ceamx/rectangle-dispatch/body

  "C-x b" #'ceamx/switch-to-buffer
  "C-x g" #'magit-status
  "C-x l" #'ialign

  "C-x k" #'ceamx-simple/kill-current-buffer
  "C-x K" #'kill-buffer
  "C-x u" #'vundo

  "C-x n N" #'logos-narrow-dwim

  "C-x r b" #'consult-bookmark

  "C-x o"  #'crux-other-window-or-switch-buffer

  "C-x x o" #'olivetti-mode
  "C-x x f" #'follow-mode
  "C-x x l" #'visual-line-mode
  "C-x x r" #'rename-uniquely

  "C-x <up>" #'enlarge-window
  "C-x <down>" #'shrink-window
  "C-x <left>" #'shrink-window-horizontally
  "C-x <right>" #'enlarge-window-horizontally

  "C-x C-b" #'ibuffer
  "C-x C-n" #'next-buffer
  "C-x C-p" #'previous-buffer

  ;; Minimizing frames is the job of the window manager.
  "C-x C-z" nil

  ;; Since `comment-dwim' is bound to [M-;], I find it unintuitive
  ;; that `comment-line' is bound to [C-x C-;].
  "C-x M-;" #'comment-line

  "C-x M-:" #'consult-complex-command
  "C-x M-g" #'magit-dispatch)

;; [C-x 4] :: Window Prefix :window:


(define-keymap :keymap (current-global-map)
  "C-x 4 b" #'consult-buffer-other-window
  "C-x 4 t" #'crux-transpose-windows)

;; [C-x 5] :: Frame Prefix :frame:


(define-keymap :keymap (current-global-map)
  "C-x 5 b" #'consult-buffer-other-frame)

;; [C-x 8] :: Character Prefix

(define-keymap :keymap (current-global-map)
  "C-x 8 i" (cons "icons" (define-prefix-command 'ceamx-insert-icons-prefix 'ceamx-insert-icons-prefix-map))
  "C-x 8 i i" #'nerd-icons-insert)

;; [C-x p] :: Project Prefix


(define-keymap :keymap (current-global-map)
  "C-x p ." #'project-dired
  "C-x p RET" #'project-dired
  "C-x p DEL" #'project-forget-project

  "C-x p b" #'consult-project-buffer)

;; [C-x t] :: Tab Prefix


(define-keymap :keymap (current-global-map)
  "C-x t b" #'consult-buffer-other-tab)

;; [C-x v] :: Version Control Prefix :vcs:


(define-keymap :keymap (current-global-map)
  "C-x v ." #'vc-dir-root
  "C-x v RET" #'vc-dir-root

  "C-x v B" #'vc-annotate
  "C-x v d" #'vc-diff
  "C-x v e" #'vc-ediff
  "C-x v G" #'vc-log-search
  "C-x v o" #'forge-browse-commit
  "C-x v t" #'git-timemachine)

(after! vc-dir
  (define-keymap :keymap vc-dir-mode-map
    "d" #'vc-diff
    "o" #'vc-dir-find-file-other-window
    "O" #'vc-log-outgoing))

;; [C-x w] :: Window Manipulation Prefix :window:


(define-keymap :keymap (current-global-map)
  "C-x w w" #'ace-window

  "C-x w SPC" #'transpose-frame

  "C-x w d" #'ace-delete-window
  "C-x w p" #'popper-toggle
  "C-x w P" #'popper-toggle-type
  "C-x w u" #'winner-undo
  "C-x w U" #'winner-redo

  "C-x w h" #'windmove-left
  "C-x w H" #'ceamx/window-move-left
  "C-x w j" #'windmove-down
  "C-x w J" #'ceamx/window-move-down
  "C-x w k" #'windmove-up
  "C-x w K" #'ceamx/window-move-up
  "C-x w l" #'windmove-right
  "C-x w L" #'ceamx/window-move-right

  "C-x w =" #'balance-windows
  "C-x w <" #'flip-frame
  "C-x w >" #'flop-frame
  "C-x w [" #'rotate-frame-clockwise
  "C-x w ]" #'rotate-frame-anticlockwise
  "C-x w {" #'rotate-frame
  "C-x w }" #'rotate-frame)

;; [M-] :: Global Meta-Modified


(define-keymap :keymap (current-global-map)
  "M-#" #'consult-register-load
  "M-$" #'jinx-correct
  "M-*" #'tempel-insert
  "M-=" #'count-words
  "M-+" #'tempel-complete
  "M-]" #'logos-forward-page-dwim
  "M-[" #'logos-backward-page-dwim
  "M-'" #'consult-register-store
  "M-." #'embark-dwim

  "M-DEL" #'ceamx/backward-kill-word
  "M-SPC" #'cycle-spacing
  "M-<up>" #'drag-stuff-up
  "M-<right>" #'drag-stuff-right
  "M-<down>" #'drag-stuff-down
  "M-<left>" #'drag-stuff-left

  "M-c" #'capitalize-dwim
  "M-f" #'forward-word
  "M-F" #'forward-symbol
  ;; FIXME: reconcile with current binding for `forward-symbol'
  ;; "M-F" #'consult-focus-lines
  "M-k" #'ceamx-simple/kill-line-backward
  "M-K" #'consult-keep-lines
  "M-l" #'downcase-dwim
  "M-o" #'crux-other-window-or-switch-buffer
  "M-O" #'delete-blank-lines
  "M-q" #'unfill-toggle
  "M-Q" #'repunctuate-sentences
  "M-u" #'upcase-dwim
  "M-w" #'easy-kill
  "M-y" #'consult-yank-pop
  "M-z" #'zap-up-to-char
  "M-Z" #'ceamx-simple/zap-to-char-backward)

(keymap-set minibuffer-local-map "M-r" #'consult-history) ; orig. `previous-matching-history-element'

(define-keymap :keymap prog-mode-map
  "M-RET" #'ceamx-simple/continue-comment)

(after! puni
  (define-keymap :keymap puni-mode-map
    "M-(" #'puni-syntactic-forward-punct
    "M-)" #'puni-syntactic-backward-punct))

;; TODO: testing disabling this
;; (keymap-substitute (current-global-map) #'default-indent-new-line #'ceamx-simple/continue-comment)

;; [M-g] :: Goto Map :search:


(define-keymap :keymap (current-global-map)
  "M-g d" #'ceamx/dogears-dispatch
  "M-g e"  #'consult-compile-error
  "M-g f"  #'consult-flycheck            ; or: `consult-flymake'
  "M-g g"  #'consult-goto-line
  "M-g M-g" #'consult-goto-line
  "M-g o"  #'consult-outline
  "M-g m"  #'consult-mark
  "M-g k"  #'consult-global-mark
  "M-g i"  #'consult-imenu
  "M-g I"  #'consult-imenu-multi
  "M-g u" #'link-hint-open-link
  "M-g U" #'link-hint-copy-link
  "M-g w" #'avy-goto-word-1

  ;; Also see `ceamx/dogears-dispatch'.
  "M-g M-b" #'dogears-back
  "M-g M-f" #'dogears-forward
  "M-g M-d" #'dogears-list
  "M-g M-D" #'dogears-sidebar)

(after! org-mode
  (keymap-set org-mode-map "M-g o" #'consult-org-heading))

;; [M-s] :: “Search” Prefix :search:


(define-keymap :keymap (current-global-map)
  "M-s b" #'ceamx-simple/buffers-major-mode
  "M-s c"  #'consult-locate
  "M-s d"  #'consult-fd                 ; or `consult-find'
  "M-s e"  #'consult-isearch-history
  "M-s g"  #'consult-ripgrep
  "M-s G"  #'consult-git-grep
  "M-s k"  #'consult-keep-lines
  "M-s l"  #'consult-line
  "M-s L"  #'consult-line-multi
  "M-s n" #'consult-notes
  "M-s r" '("replace..." . ceamx-replace-prefix)
  "M-s u"  #'consult-focus-lines
  "M-s v" #'ceamx-simple/buffers-vc-root

  "M-s M-f" #'org-node-find
  "M-s M-o" #'multi-occur
  "M-s M-s" #'consult-outline)

(define-keymap :keymap ceamx-replace-prefix
  "b" #'substitute-target-in-buffer
  "d" #'substitute-target-in-defun
  "s" #'substitute-target-above-point
  "S" #'substitute-target-below-point)

(after! isearch
  (define-keymap :keymap isearch-mode-map
    "M-s e" #'consult-isearch-history
    "M-s l" #'consult-line
    "M-s L" #'consult-line-multi))

(keymap-set minibuffer-local-map "M-s" #'consult-history)

(after! org-mode
  (keymap-set org-mode-map "M-s M-i" #'org-node-insert-link))

;; “Mouse” Input


(define-keymap :keymap (current-global-map)
  "<wheel-left>" #'scroll-left
  "<wheel-right>" #'scroll-right)

;; Repeat Maps


(defvar-keymap ceamx-string-repeat-map
  :repeat t

  "c" #'ceamx/cycle-string-inflection)

;; Window :window:


(define-keymap :keymap resize-window-repeat-map
  "<up>" #'enlarge-window
  "<down>" #'shrink-window
  "<left>" #'shrink-window-horizontally
  "<right>" #'enlarge-window-horizontally)

(defvar-keymap ceamx-window-transposition-repeat-map
  :repeat t

  "SPC" #'transpose-frame
  "<" #'flip-frame
  ">" #'flop-frame
  "[" #'rotate-frame-clockwise
  "]" #'rotate-frame-anticlockwise)

(defvar-keymap ceamx-window-lifecycle-repeat-map
  :repeat t

  "2" #'split-window-below
  "3" #'split-window-right
  "o" #'ace-window)

;; Start the Emacs server process if not already running


(def-hook! ceamx-init-maybe-start-server-h ()
  'ceamx-emacs-startup-hook
  "Allow this Emacs process to act as server if a server is not already running."
  (require 'server)
  (unless (server-running-p)
    (server-start)))

;; macOS: Restart Yabai after init

;; Otherwise, =yabai= will not "see" the Emacs GUI window.


(when (and (ceamx-host-macos-p) (display-graphic-p))
  (def-hook! ceamx-after-init-restart-yabai-h ()
    'ceamx-after-init-hook
    "Restart the yabai service after init."
    (after! exec-path-from-shell
      (async-shell-command "yabai --restart-service"))))

;; Optionally load the ~custom-file~


(when ceamx-load-custom-file
  (load custom-file t))
