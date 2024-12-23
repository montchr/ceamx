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

;; Configure cursor appearance :graphical:


(use-package cursory
  :ensure t
  :demand t
  :if (display-graphic-p)

  :preface
  (setopt cursory-latest-state-file (expand-file-name "cursory-latest-state.eld" ceamx-var-dir))

  :init
  (keymap-set ceamx-session-map "a c" #'cursory-set-preset)

  :config
  (setopt cursory-presets
          '((box
             :blink-cursor-interval 0.8)
            (box-no-blink
             :blink-cursor-mode -1)
            (bar
             :cursor-type (bar . 2)
             :blink-cursor-interval 0.8)
            (bar-no-other-window
             :inherit bar
             :cursor-in-non-selected-windows nil)
            (bar-no-blink
             :cursor-type (bar . 2)
             :blink-cursor-mode -1)
            (t
             :cursor-type box
             :cursor-in-non-selected-windows hollow
             :blink-cursor-mode 1
             :blink-cursor-blinks 10
             :blink-cursor-interval 0.2
             :blink-cursor-delay 0.2)))

  (cursory-set-preset (or (cursory-restore-latest-preset) 'box))

  (cursory-mode 1))

;; Customize the Customization buffers and menus


(setopt custom-theme-allow-multiple-selections nil)

(setopt custom-unlispify-menu-entries nil)
(setopt custom-unlispify-tag-names nil)
(setopt custom-unlispify-remove-prefixes nil)

(add-hook 'Custom-mode-hook #'custom-toggle-hide-all-widgets nil t)

;; =grid=: Library for textual data table presentation :package:

;; - Source :: [[https://github.com/ichernyshovvv/grid.el][ichernyshovvv/grid.el]]
;; - Retrieved :: [2024-06-07 Fri 11:45]

;; #+begin_quote
;; This library allows you to put text data into boxes and align them horizontally,
;; applying margin, padding, borders.
;; #+end_quote


(package! (grid :host github :repo "ichernyshovvv/grid.el"))

;; =hydra= :package:hydra:

;; - Documentation :: <https://github.com/jerrypnz/major-mode-hydra.el/#pretty-hydra>


(package! hydra)
(package! pretty-hydra)

;; =transient= :package:transient:


(package! transient
  ;; Restore the default location, overriding `no-littering'.  I consider these
  ;; values configuration to be exposed, not state to be hidden.  See
  ;; `transient-save-values' and related.
  (setopt transient-values-file (locate-user-emacs-file "transient/values.el")))

(package! magit-section)

;; Close any ~transient~ menu with the escape key


(with-eval-after-load 'transient
  (keymap-set transient-map "<escape>" #'transient-quit-one))

;; Load the library functions


(require 'lib-ui)

;; Define customizable options relating to themes


(defcustom ceamx-ui-theme-family 'modus
  "Set of themes to load.
Valid values are the symbols `ef', `modus', and `standard', which
reference the `ef-themes', `modus-themes', and `standard-themes',
respectively.

A nil value does not load any of the above (use Emacs without a
theme)."
  :group 'ceamx
  :type '(choice :tag "Set of themes to load" :value modus
          (const :tag "The `ef-themes' module" ef)
          (const :tag "The `modus-themes' module" modus)
          (const :tag "The `standard-themes' module" standard)
          (const :tag "Do not load a theme module" nil)))

(defcustom ceamx-ui-theme-light 'modus-operandi-tinted
  "The default light theme."
  :group 'ceamx
  :type 'symbol)

(defcustom ceamx-ui-theme-dark 'modus-vivendi
  "The default dark theme."
  :group 'ceamx
  :type 'symbol)

(defvar ceamx-ui-dark-themes-list nil)

(defvar ceamx-ui-light-themes-list nil)

(after! modus-themes
  (appendq! ceamx-ui-dark-themes-list
            ;; `modus-vivendi' variants
            (seq-filter
             (lambda (sym) (string-prefix-p "modus-vivendi" (symbol-name sym)))
             modus-themes-items))

  (appendq! ceamx-ui-light-themes-list
            ;; `modus-operandi' variants
            (seq-filter
             (lambda (sym) (string-prefix-p "modus-operandi" (symbol-name sym)))
             modus-themes-items)))

;; Consider all themes "safe"


(setopt custom-safe-themes t)

;; Add a custom hook ~ceamx-after-enable-theme-hook~ to run after enabling a theme

;; - Source :: <https://github.com/jdtsmith/kind-icon/issues/34#issuecomment-1668560185>


(defvar ceamx-after-enable-theme-hook nil)

(defun ceamx-after-enable-theme (&rest _args)
  "Hook to run after enabling theme."
  (run-hooks 'ceamx-after-enable-theme-hook))

(advice-add 'enable-theme :after #'ceamx-after-enable-theme)

;; Modus Themes :package:

;; - Website :: <https://protesilaos.com/modus-themes/>


(package! modus-themes
  (require 'modus-themes)

  (setopt modus-themes-italic-constructs t)
  (setopt modus-themes-bold-constructs nil)
  (setopt modus-themes-mixed-fonts t)
  (setopt modus-themes-variable-pitch-ui nil)
  (setopt modus-themes-disable-other-themes t)
  (setopt modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi))

  (setopt modus-themes-headings
          '((agenda-structure . (variable-pitch light 2.2))
            (agenda-date . (variable-pitch regular 1.3))
            (t . (regular 1.0))))

  (let ((overrides '((cursor blue)

                     ;; Syntax
                     (builtin magenta)
                     (comment red-faint)
                     (constant magenta-cooler)
                     (docstring magenta-faint)
                     (docmarkup green-faint)
                     (fnname magenta-warmer)
                     (keybind green-cooler)
                     (keyword cyan)
                     (preprocessor cyan-cooler)
                     (string red-cooler)
                     (type magenta-cooler)
                     (variable blue-warmer)
                     (rx-construct magenta-warmer)
                     (rx-backslash blue-cooler)

                     ;; Buttons
                     (bg-button-active bg-main)
                     (fg-button-active fg-main)
                     (bg-button-inactive bg-inactive)
                     (fg-button-inactive "gray50")

                     ;; Mode-line
                     (bg-mode-line-active bg-lavender)
                     (fg-mode-line-active fg-main)
                     (border-mode-line-active bg-lavender)
                     (border-mode-line-inactive unspecified)

                     ;; Fringe
                     (fringe unspecified)

                     ;; Prompts
                     ;; (fg-prompt fg-main)
                     ;; not really subtle! too loud.
                     ;; (bg-prompt bg-yellow-subtle)

                     ;; Pair-matching (parens)
                     (bg-paren-match unspecified)
                     (fg-paren-match magenta-intense)
                     (underline-paren-match magenta-intense)

                     ;; Link styles
                     ;; (underline-link border)
                     ;; (underline-link-visited border)
                     )))
    (setopt modus-operandi-palette-overrides overrides
            modus-operandi-tinted-palette-overrides overrides
            modus-vivendi-palette-overrides overrides
            modus-vivendi-tinted-palette-overrides overrides)))

;; Ef-Themes :package:

;; - Website :: <https://protesilaos.com/emacs/ef-themes>


(defvar ceamx-font-headings-style-alist)

(package! ef-themes
  (require 'ef-themes)

  (setopt ef-themes-to-toggle '(ef-night ef-frost)
          ef-themes-mixed-fonts t
          ef-themes-variable-pitch-ui nil))

;; Theme Phasing Schedule


(defcustom ceamx-ui-theme-circadian-interval 'solar
  "The circadian theme switching interval.
Value may be `period', `solar', or nil, corresponding
respectively to period-based switching with `theme-buffet' or
sunrise/sunset toggling from the combination of the `solar'
library and the `circadian' package.

A nil value means to disable automatic theme switching.
Theme-switching commands `ceamx/light' and `ceamx/dark' will
unconditionally use `ceamx-ui-theme-default-light' and
`ceamx-ui-theme-default-dark', respectively."
  :group 'ceamx
  :type '(choice :tag "Circadian theme switching interval" :value nil
          (const :tag "Time periods via `theme-buffet'" :value buffet)
          (const :tag "Sunrise or sunset via `solar' and `circadian'" :value solar)))

;; Set approximate stomping coordinates for hyper-astronomic relativity calculations


(require 'cal-dst)

(setopt calendar-latitude 39.968)
(setopt calendar-longitude -75.133)

;; =circadian=: theme phasing based on sunrise/sunset time :package:


(package! circadian
  (when (eq 'solar ceamx-ui-theme-circadian-interval)
    (setopt circadian-themes `((:sunrise . ,ceamx-ui-theme-light)
                               (:sunset . ,ceamx-ui-theme-dark)))
    (circadian-setup)))

;; FIXME
;; (after! circadian
;;   (def-hook! +circadian-after-load-theme-set-system-theme-h (theme)
;;     'circadian-after-load-theme-hook
;;     "Set the desktop environment theme based on THEME polarity."
;;     (cond
;;      ((memq theme ceamx-ui-dark-themes-list)
;;       (ceamx-ui/gsettings-dark-theme))
;;      ((memq theme ceamx-ui-light-themes-list)
;;       (ceamx-ui/gsettings-light-theme))
;;      (t nil))))

;; Elpaca-Wait № 4: ensure availability of themes for integration :wait:


(elpaca-wait)

;; Load a default theme

;; Configure some user options dependent on the loaded packages:


(setopt ceamx-ui-theme-light 'modus-operandi-tinted)
(setopt ceamx-ui-theme-dark 'modus-vivendi)

(if (eq 'solar ceamx-ui-theme-circadian-interval)
    (after! circadian (add-hook 'ceamx-after-init-hook #'circadian-setup))
  (if (ceamx-ui-desktop-dark-theme-p)
      (ceamx-ui/load-dark-theme)
    (ceamx-ui/load-light-theme)))

;; Avy :package:

;; - Website :: <https://github.com/abo-abo/avy>
;; - Ref :: <https://karthinks.com/software/avy-can-do-anything/>


(package! avy
  ;; Reduce the number of possible candidates.
  ;; Can be overridden with the universal argument.
  (setopt avy-all-windows nil)
  ;; Prevent conflicts with themes.
  (setopt avy-background nil)
  (setopt avy-style 'at-full)
  ;; Anything lower feels unusable.
  (setopt avy-timeout-seconds 0.25)

  (keymap-global-set "M-j" #'avy-goto-char-timer)

  (after! lispy
    (defvar lispy-mode-map)
    (declare-function lispy-join "lispy")
    ;; Prevent conflict with newly-added M-j binding.
    (keymap-set lispy-mode-map "M-J" #'lispy-join)))

;; ~rainbow-mode~: Colorize color names and hexcodes in buffers :theme:

;; <https://elpa.gnu.org/packages/rainbow-mode.html>


(package! rainbow-mode)

;; Highlight the current line with ~hl-line-mode~ [builtin]


(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; Differentiate between focused and non-focused windows :window:


(setopt highlight-nonselected-windows nil)

;; =pulsar=: pulse current line after function invocations :package:animation:


(package! pulsar
  (pulsar-global-mode 1)

  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line))

(after! pulsar
  (setopt pulsar-pulse t
          pulsar-delay 0.055
          pulsar-iterations 10
          pulsar-face 'pulsar-magenta
          pulsar-highlight-face 'pulsar-cyan)

  (dolist (fn '(pulsar-pulse-line-red pulsar-recenter-top pulsar-reveal-entry))
    (add-hook 'next-error-hook (function fn))))

;; Allow restoring deleted frames


(undelete-frame-mode 1)

;; macOS: Configure frame decorations :graphical:macos:


(unless (ceamx-host-macos-p)
  ;; Hide window decorations.
  (add-to-list 'default-frame-alist '(undecorated . t)))

;; Handle macOS-specific workarounds :macos:


(when (ceamx-host-macos-p)
  ;; `undecorated-round' is macOS-specific.
  (add-to-list 'default-frame-alist '(undecorated-round . t))

  ;; GUI menu bar is necessary otherwise Emacs will be treated as a
  ;; non-application OS window (e.g. no focus capture).
  ;; <https://github.com/doomemacs/doomemacs/blob/d657be1744a1481dc4646d0b62d5ee1d3e75d1d8/lisp/doom-start.el#L118-L128>
  (def-hook! ceamx-frame--maybe-restore-gui-menu-bar-h (&optional frame)
    '(after-make-frame-functions window-setup-hook)
    "TODO: Provide source for this approach (Doom?), and why it does what it does."
    (when-let (frame (or frame (selected-frame)))
      (when (display-graphic-p frame)
        (set-frame-parameter frame 'menu-bar-lines 1))))

  ;; Stop C-z from minimizing windows.
  (keymap-global-unset "C-z" t))

;; Provide a more comfortably-spaced Emacs layout density with =spacious-padding= :package:graphical:


(use-package spacious-padding
  :if (display-graphic-p)
  :hook (ceamx-after-init . spacious-padding-mode)

  :init
  (setopt spacious-padding-widths '( :internal-border-width 30
                                     :header-line-width 4
                                     :mode-line-width 6
                                     :tab-width 4
                                     :right-divider-width 30
                                     :scroll-bar-width 8
                                     :left-fringe-width 20
                                     :right-fringe-width 20))

  (setopt spacious-padding-subtle-mode-line
    `( :mode-line-active default
       :mode-line-inactive window-divider)))

;; Menu Bar :menubar:

;; Disable the menu bar by default:


(menu-bar-mode -1)



;; But allow toggling it manually:


(keymap-set ceamx-toggle-map "M" #'menu-bar-mode)

;; Enable ~tab-bar-mode~ in Emacs 30

;; - ref :: <https://lists.gnu.org/r/bug-gnu-emacs/2023-07/msg01594.html>

;; ~tab-bar-mode~ is currently broken in Emacs 29 due to upstream bug.  The fix is
;; present on the =master= branch (Emacs 30), but it will not be backported.

;; Unfortunately, the bug is impossibly distracting.  So I am avoiding
;; `tab-bar-mode' on Emacs 29.

;; As of <2024-06-06>, I am using the =nix-community/emacs-overlay#emacs-pgtk= package tracking the
;; Emacs =master= branch.  ~tab-bar-mode~ is that important to me.  Emacs 30 seems
;; stable enough so far.


(unless (version< emacs-version "30")
  (tab-bar-mode 1))

(keymap-set ceamx-toggle-map "T" #'tab-bar-mode)

;; Configure tab bar appearance and behavior


(setopt tab-bar-auto-width t
        tab-bar-auto-width-max '((80) 10))

;; Define a custom setting to adjust font height multiplier


(defcustom ceamx-font-height-multiplier 1.0
  "Multiplier for display font size.
Intended for use as a per-system (or, ideally, per-display)
accommodation for varying pixel densities."
  :group 'ceamx
  :type '(float))

;; Text rendering and scaling


(setq x-underline-at-descent-line nil)

(setq-default text-scale-remap-header-line t)

;; ~fontaine~: pre-configure font presets :package:

;; <https://protesilaos.com/emacs/fontaine>

;; TIP: You can test out alterations quickly with, for example:
;;      (internal-set-lisp-face-attribute 'default :weight 'semilight)


(use-package fontaine
  :ensure (:wait t)
  :demand t
  :if (display-graphic-p)

  :init
  (setopt fontaine-latest-state-file (expand-file-name "fontaine-latest-state.eld" ceamx-var-dir))

  ;; For some reason I do not yet understand, according to some hearsay, font
  ;; sizes best scale in multiples of 3-point increments. So, each height value
  ;; is a multiple of 3.
  (setopt fontaine-presets
          `( (tiny
              :bold-weight medium
              :default-height ,(pcase (system-name)
                                (_ 78))
              :default-weight ,(pcase (system-name)
                                (_ 'semilight)))
             (small
              :bold-weight medium
              :default-height ,(pcase (system-name)
                                (_ 90))
              :default-weight ,(pcase (system-name)
                                (_ 'regular)))
             (regular)
             (medium
              :default-height ,(pcase (system-name)
                                ("boschic" 124)
                                ("tuuvok"
                                 120
                                 ;; 115

                                 )
                                (_ 120)))
             (large
              :default-height ,(pcase (system-name)
                                ;; ("tuuvok" 140)
                                (_ 144))
              :default-weight semilight
              :bold-weight semibold)
             (xlarge
              :default-height ,(pcase (system-name)
                                (_ 156))
              :bold-weight bold)
             (big-mclarge-huge
              :default-weight semilight
              :default-height ,(pcase (system-name)
                                (_ 180))
              :bold-weight extrabold)
             (t
              :default-family "Iosevka Comfy"
              :default-weight regular
              :default-slant normal
              :default-height ,(pcase (system-name)
                                ("tuuvok" 102)
                                (_ 105))

              :fixed-pitch-family "Iosevka Comfy"
              :fixed-pitch-weight nil
              :fixed-pitch-slant nil
              :fixed-pitch-height 1.0

              :fixed-pitch-serif-family nil
              :fixed-pitch-serif-weight nil
              :fixed-pitch-serif-slant nil
              :fixed-pitch-serif-height 1.0

              :variable-pitch-family "Iosevka Comfy Motion"
              :variable-pitch-weight nil
              :variable-pitch-slant nil
              :variable-pitch-height 1.0

              :header-line-family nil
              :header-line-height 1.0
              :header-line-slant nil
              :header-line-weight nil

              :line-number-family nil
              :line-number-height 1.0
              :line-number-slant nil
              :line-number-weight nil

              :mode-line-active-family nil
              :mode-line-active-weight nil
              :mode-line-active-slant nil
              :mode-line-active-height 1.0

              :mode-line-inactive-family nil
              :mode-line-inactive-weight nil
              :mode-line-inactive-slant nil
              :mode-line-inactive-height 1.0

              :tab-bar-family nil
              :tab-bar-weight nil
              :tab-bar-slant nil
              :tab-bar-height 1.0

              :tab-line-family nil
              :tab-line-weight nil
              :tab-line-slant nil
              :tab-line-height 1.0

              :bold-family nil
              :bold-weight medium
              ;; :bold-weight semibold
              :bold-slant nil
              :bold-height 1.0

              :italic-family nil
              :italic-weight nil
              :italic-slant italic
              :italic-height 1.0

              :line-spacing 1)))

  :config
  ;; Persist latest preset across sessions.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))

;; ~ligature.el~: improved ligature support :package:

;; <https://github.com/mickeynp/ligature.el>

;; A better implementation of ligature support than the builtin ~prettify-symbols-mode~.
;; <https://old.reddit.com/r/emacs/comments/keji66/what_is_bad_about_prettifysymbolsmode/>


(use-package ligature
  :ensure t
  :demand t
  :if (display-graphic-p)
  :after fontaine

  :config
  ;; Enable all Iosevka ligatures in programming modes.
  ;; <https://github.com/mickeynp/ligature.el/wiki#iosevka>
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))

  (global-ligature-mode t))

;; Provide common dependency: ~nerd-icons~ :package:


(package! nerd-icons
  (setopt nerd-icons-font-family "Symbols Nerd Font Mono")
  (require 'nerd-icons))

;; Provide common dependency: ~svg-lib~ :package:


(package! svg-lib)

;; ~page-break-lines~: Improve appearance of form feed characters :package:

;; - docs :: <https://github.com/purcell/page-break-lines/blob/master/README.md>


(package! page-break-lines
  (global-page-break-lines-mode))

;; Modeline :modeline:


(defcustom ceamx-modeline-provider nil
  "Modeline provider to load.
Valid values are the symbols `doom', `nano', and `telephone'
which reference the `doom-modeline', `nano-modeline', and
`telephone-line' modules respectively.

A nil value will not load any modeline customizations (use Emacs
with its default modeline)."
  :group 'ceamx
  :type '(choice :tag "Modeline to load" :value nil
          (const :tag "The `doom-modeline' module" doom)
          (const :tag "The `nano-modeline' module" nano)
          (const :tag "The `telephone-line' module" telephone)
          (const :tag "Do not load a modeline module" nil)))

(line-number-mode 1)
(column-number-mode 1)

(setopt display-time-24hr-format t)

;; Show current command and its binding with ~keycast~

;; - Website :: <https://github.com/tarsius/keycast>

;; Supports display in the mode-line, header-line, tab-bar, and as messages in a
;; dedicated frame.

;; NOTE: Incompatible with kitchen-sink modeline packages like =doom-modeline= and
;; =telephone-line=.


(package! keycast
  (keymap-set ceamx-toggle-map "k" #'keycast-mode-line-mode))

(after! keycast
  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p mouse-movement-p mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil))))

;; =olivetti=: "Distraction-free" editing :package:

;; <https://github.com/rnkn/olivetti>


(use-package olivetti
  :ensure t
  :commands (olivetti-mode)
  :config
  (setopt olivetti-body-width 0.7
    olivetti-minimum-body-width 80
    olivetti-recall-visual-line-mode-entry-state t))

;; =logos=: a simple focus mode with page breaks or outlines :package:


(use-package logos
  :ensure t
  :init
  (define-keymap :keymap (current-global-map)
    "C-x n n" #'logos-narrow-dwim
    "C-x ]" #'logos-forward-page-dwim
    "C-x [" #'logos-backward-page-dwim
    "M-]" #'logos-forward-page-dwim
    "M-[" #'logos-backward-page-dwim)

  (keymap-set ceamx-toggle-map "z" #'logos-focus-mode)

  :config
  (setopt logos-outlines-are-pages t)
  (setopt logos-outline-regexp-alist
    `((emacs-lisp-mode . ,(format "\\(^;;;+ \\|%s\\)" logos-page-delimiter))
      (org-mode . ,(format "\\(^\\*+ +\\|^-\\{5\\}$\\|%s\\)" logos-page-delimiter))
      (markdown-mode . ,(format "\\(^\\#+ +\\|^[*-]\\{5\\}$\\|^\\* \\* \\*$\\|%s\\)" logos-page-delimiter))
      (conf-toml-mode . "^\\[")))

  ;; These apply buffer-locally when `logos-focus-mode' is enabled.
  (setq-default logos-hide-mode-line t)
  (setq-default logos-hide-header-line t)
  (setq-default logos-hide-buffer-boundaries t)
  (setq-default logos-hide-fringe t)
  (setq-default logos-buffer-read-only nil)
  (setq-default logos-scroll-lock nil)

  (when (display-graphic-p)
    (setq-default logos-variable-pitch t))

  (when (fboundp 'olivetti-mode)
    (setq-default logos-olivetti t))

  (add-hook 'enable-theme-functions #'logos-update-fringe-in-buffers)

  (def-hook! ceamx-logos--recenter-top ()
    '(logos-page-motion-hook)
    "Place point at the top when changing pages in non-`prog-mode' modes."
    (unless (derived-mode-p 'prog-mode)
      ;; NOTE: '0' value will recenter at the absolute top.
      (recenter 1))))

;; Keybindings :keybinds:


(define-keymap :keymap ceamx-session-map
  "a" (cons "Appearance" (define-prefix-command 'ceamx-session-appearance-prefix-command))
  "a f" #'fontaine-set-preset
  "a d" #'ceamx-ui/dark
  "a l" #'ceamx-ui/light
  "a o" #'olivetti-mode

  "f" (cons "Frame" (define-prefix-command 'ceamx-session-f-prefix))
  "f d" #'delete-frame)

;; =init.el=: Load Features
;; :PROPERTIES:
;; :header-args: :tangle init.el
;; :END:


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
