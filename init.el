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
;; :PROPERTIES:
;; :ID:       8faa0cb3-1b4d-4218-a2c3-0016aba1d95b
;; :END:


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
;; :PROPERTIES:
;; :ID:       7f77da4e-3109-4dca-80c1-38c838ee5d70
;; :END:


(require 'on)

;; Elpaca
;; :PROPERTIES:
;; :ID:       af51c154-98f0-487d-ba00-90db3d1d032f
;; :END:


(defvar elpaca-directory (expand-file-name "elpaca/" ceamx-packages-dir))

;; Avoid aggressive GitHub API rate limiting.
(defvar elpaca-queue-limit 30)



;; Elpaca needs to know about the Nix build date of the current version
;; of Emacs to set ~elpaca-core-date~ correctly.  [[https://github.com/progfolio/elpaca/wiki/Usage-with-Nix#retrieving-the-date-via-file-name][From the wiki]]:


(require 'ceamx-lib)

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


;; (unless after-init-time
;;   (when (featurep 'org)
;;     (unload-feature 'org)))

;; (elpaca (org :autoloads "org-loaddefs.el"))

;; Install the latest version of ~use-package~
;; :PROPERTIES:
;; :ID:       87854bdb-fb6f-4117-884e-81354c924c07
;; :END:


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
;; :PROPERTIES:
;; :ID:       863979e9-5559-4abf-903e-307f528e6263
;; :END:


(def-advice! ceamx+use-package--bind-handle-sharp-quotes-a (args)
  :filter-args #'use-package-normalize-binder
  "Make `use-package' handle sharp-quoted functions correctly in `:bind'.
Performs a recursive find-and-replace on sharp quotes in the arguments,
because that's the simple solution and the performance overhead is
unimportant since it happens during compilation anyway."
  (ceamx--remove-sharp-quotes args))

;; Install and configure =setup.el=
;; :PROPERTIES:
;; :ID:       d3695516-a695-489d-8a4f-374e54e59e31
;; :END:


(elpaca setup
  (require 'setup))

(elpaca-wait)



;; Add Elpaca support to =setup.el=:


(defun +setup-wrap-to-install-elpaca-package (body _name)
  "Wrap BODY in an `elpaca' block when `:ensure' is provided."
  (if (assq 'ensure setup-attributes)
      `(elpaca ,(cdr (assq 'ensure setup-attributes))
         ,@(macroexp-unprogn body))
    body))

(add-to-list 'setup-modifier-list #'+setup-wrap-to-install-elpaca-package)

(setup-define :ensure
  (lambda (order &rest recipe)
    (push (cond
           ((eq order t) `(ensure . ,(setup-get 'feature)))
           ((eq order nil) `(ensure . nil))
           (`(ensure . (,order ,@recipe))))
          setup-attributes)
    ;; If the macro returned non-nil, it would try to insert the
    ;; modified list returned by `push'.  As this value usually cannot
    ;; be evaluated, it is better to return nil (which the byte
    ;; compiler will optimize away).
    nil)
  :documentation "Install ORDER with the `elpaca' package manager.
The ORDER can be used to deduce the feature context."
  :shorthand #'cadr)



;; Add the =:autoload= contextual macro:


;; <https://www.emacswiki.org/emacs/SetupEl#h5o-7>
(setup-define :autoload
  (lambda (func)
    (let ((fn (if (memq (car-safe func) '(quote function))
                  (cadr func)
                func)))
      `(unless (fboundp (quote ,fn))
         (autoload (function ,fn) ,(symbol-name (setup-get 'feature)) nil t))))
  :documentation "Autoload COMMAND if not already bound."
  :debug '(form)
  :repeatable t
  :signature '(FUNC ...))



;; Add the =:load-after= contextual macro:


(setup-define :load-after
    (lambda (&rest features)
      (let ((body `(require ',(setup-get 'feature))))
        (dolist (feature (nreverse features))
          (setq body `(with-eval-after-load ',feature ,body)))
        body))
    :documentation "Load the current feature after FEATURES.")

;; =gcmh=: manage running garbage collection on idle :package:perf:

;; - Website :: <https://akrl.sdf.org/>
;; - Code :: <https://gitlab.com/koral/gcmh>

;; During normal use, the GC threshold will be set to a high value.
;; When idle, GC will be triggered with a low threshold.


(package! gcmh
  (add-hook 'ceamx-emacs-startup-hook #'gcmh-mode))

;; Install utility libraries
;; :PROPERTIES:
;; :ID:       d04ef910-34e0-4961-aeb6-c2741af41455
;; :END:


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
;; :PROPERTIES:
;; :ID:       9e6bd5ec-eef5-4219-9e26-75a910fd845b
;; :END:


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
;; :PROPERTIES:
;; :ID:       a0759984-15e9-4ccb-b5e7-ac7cfa6073e6
;; :END:


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
;; :PROPERTIES:
;; :ID:       4572e438-3d90-4ed3-88af-a7c2493a2e19
;; :END:

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
;; :PROPERTIES:
;; :ID:       d8b4dda2-794a-4498-8db8-e2f4873dc0fd
;; :END:


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

;; =init.el= :: Leader Prefix Keybindings
;; :PROPERTIES:
;; :header-args: :tangle init.el
;; :ID:       cd713eda-629c-460c-921e-d99ce8f6742f
;; :END:


(define-keymap :keymap (current-global-map)
  "C-c a" #'org-agenda
  "C-c b" (cons "[ BUFFER    ]" #'ceamx-buffer-prefix)
  "C-c c" (cons "[ CAPTURE   ]" #'ceamx-capture-prefix)
  ;; "C-c d"
  "C-c e" (cons "[ EDIT      ]" #'ceamx-structural-editing-prefix)
  "C-c f" (cons "[ FILE      ]" #'ceamx-file-prefix)
  "C-c g" #'magit-file-dispatch
  "C-c h" #'consult-history
  "C-c i" (cons "[ INSERT    ]" #'ceamx-insert-prefix)
  ;; "C-c j"
  "C-c k" #'consult-kmacro
  ;; TODO: disambiguate?
  "C-c K" (cons "[ CRYPTION  ]" #'ceamx-cryption-prefix)
  "C-c l" (cons "[ CODE      ]" #'ceamx-code-prefix)
  ;; TODO: disambiguate f/F
  "C-c l f" (cons "folding..." (define-prefix-command 'ceamx-code-f-prefix))
  "C-c l F" (cons "formatting..." (define-prefix-command 'ceamx-code-F-prefix))
  "C-c m" (cons "[ BOOKMARK  ]" #'ceamx-bookmark-prefix)
  "C-c n" (cons "[ NOTES     ]" #'ceamx-note-prefix)
  "C-c o" (cons "[ LAUNCH    ]" #'ceamx-launch-prefix)
  "C-c p" #'completion-at-point
  "C-c P" (cons "[ COMPLETE  ]" #'ceamx-completion-prefix)
  "C-c q" (cons "[ SESSION   ]" #'ceamx-session-prefix)
  ;; "C-c r"
  ;; "C-c s"
  "C-c t" (cons "[ TOGGLE    ]" #'ceamx-toggle-prefix)
  ;; "C-c u"
  ;; "C-c v"
  "C-c w" (cons "[ WEB       ]" #'ceamx-web-prefix)
  ;; FIXME: something else... (btw this is originally set somewhere else...)
  "C-c x" #'ceamx/macrostep-expand
  ;; "C-c y"
  "C-c z" (cons "[ WORKSPACE ]" #'ceamx-workspace-prefix))

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

;; [C-c O] :: Launch
;; :PROPERTIES:
;; :ID:       bb8d84ed-5854-4892-9b76-77e67063a71e
;; :END:


(define-keymap :keymap ceamx-launch-prefix
  "s" #'scratch-buffer)

;; [C-c Q] :: Session
;; :PROPERTIES:
;; :ID:       89f0be09-aa04-40cd-9365-f41a5fbf37c6
;; :END:


(define-keymap :keymap ceamx-session-prefix
  "a c" #'cursory-set-preset
  "a d" #'ceamx-ui/dark
  "a f" #'fontaine-set-preset
  "a l" #'ceamx-ui/light
  "a o" #'olivetti-mode

  "p f" #'elpaca-fetch-all
  "p m" #'elpaca-merge-all
  "p t" #'elpaca-try

  "q" #'save-buffers-kill-emacs
  "Q" #'kill-emacs
  "r" #'restart-emacs)

;; (use-feature! ceamx-ui
;;   :commands (ceamx-ui/dark ceamx-ui/light)
;;   :config
;;   (define-keymap :keymap ceamx-session-prefix
;;     "a d" #'ceamx-ui/dark
;;     "a l" #'ceamx-ui/light))

;; [C-c T] :: Toggle


(define-keymap :keymap ceamx-toggle-prefix
  "f" #'flycheck-mode
  "k" #'keycast-mode-line-mode
  "l" #'display-line-numbers-mode
  "M" #'menu-bar-mode
  "o" #'outline-minor-mode
  "T" #'tab-bar-mode
  "w" #'window-toggle-side-windows
  "W" #'toggle-window-dedicated
  "z" #'logos-focus-mode)

;; Window
;; :PROPERTIES:
;; :ID:       93d96a59-5e99-4939-8691-14aaaf942adb
;; :END:


(define-keymap :keymap ceamx-window-prefix
  "w" #'ceamx/window-dispatch)

(define-keymap :keymap window-prefix-map
  "w" #'ace-window

  "d" #'ace-delete-window
  "p" #'popper-toggle
  "P" #'popper-toggle-type
  "u" #'winner-undo
  "U" #'winner-redo

  "h" #'windmove-left
  "H" #'ceamx/window-move-left
  "j" #'windmove-down
  "J" #'ceamx/window-move-down
  "k" #'windmove-up
  "K" #'ceamx/window-move-up
  "l" #'windmove-right
  "L" #'ceamx/window-move-right

  "=" #'balance-windows
  "<" #'flip-frame
  ">" #'flop-frame
  "[" #'rotate-frame-clockwise
  "]" #'rotate-frame-anticlockwise
  "{" #'rotate-frame
  "}" #'rotate-frame)

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
