;;; init.el --- Initialize Ceamx  -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>

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

;;;; Requirements

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

;; Add the =site-lisp= directory to ~load-path~


(add-to-list 'load-path ceamx-site-lisp-dir)
(prependq! load-path (ceamx-subdirs ceamx-site-lisp-dir))

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

;; Run our custom init and startup hooks on ~elpaca-after-init-hook~


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

;; Configure cursor appearance


(use-package cursory
  :ensure t
  :demand t
  :if (display-graphic-p)

  :preface
  (setopt cursory-latest-state-file (expand-file-name "cursory-latest-state.eld" ceamx-var-dir))

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

;; =transient=


(use-package transient
  :ensure t
  :demand t
  :config
  (keymap-set transient-map "<escape>" #'transient-quit-one))

;; =magit-section=


(use-package magit-section
  :ensure t)

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


(use-package modus-themes
  :ensure t
  :demand t

  :config
  (setopt modus-themes-italic-constructs t
          modus-themes-bold-constructs t
          modus-themes-mixed-fonts t
          modus-themes-variable-pitch-ui nil)

  (setopt modus-themes-to-toggle
          '(modus-operandi-tinted modus-vivendi))
  (setopt modus-themes-disable-other-themes t)

  (setopt modus-themes-headings
          '((agenda-structure . (variable-pitch light 2.2))
            (agenda-date . (variable-pitch regular 1.3))
            (t . (regular 1.1))))

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
  (setopt avy-timeout-seconds 0.25))

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

  :config
  (setopt logos-outlines-are-pages t)
  (setopt logos-outline-regexp-alist
    `((emacs-lisp-mode . ,(format "\\(^;;;+ \\|%s\\)" logos-page-delimiter))
      (org-mode . ,(format "\\(^\\*+ +\\|^-\\{5\\}$\\|%s\\)" logos-page-delimiter))
      (markdown-mode . ,(format "\\(^\\#+ +\\|^[*-]\\{5\\}$\\|^\\* \\* \\*$\\|%s\\)" logos-page-delimiter))
      (conf-toml-mode . "^\\[")))

  ;; These apply buffer-locally when `logos-focus-mode' is enabled.
  (setq-default logos-hide-cursor t
                logos-hide-mode-line t
                logos-hide-header-line t
                logos-hide-buffer-boundaries t
                logos-hide-fringe t
                logos-variable-pitch nil
                logos-buffer-read-only nil
                logos-scroll-lock nil
                logos-olivetti t)

  (add-hook 'enable-theme-functions #'logos-update-fringe-in-buffers)

  (def-hook! ceamx-logos--recenter-top ()
    '(logos-page-motion-hook)
    "Place point at the top when changing pages in non-`prog-mode' modes."
    (unless (derived-mode-p 'prog-mode)
      ;; NOTE: '0' value will recenter at the absolute top.
      (recenter 1))))

;; Enable some commands that Emacs disables by default


(dolist (cmd '(downcase-region
               scroll-left
               upcase-region))
  (put cmd 'disabled nil))

;; Configure sane window-scrolling behavior


(use-feature! window
  :bind
  ("C-x <" . scroll-right)
  ("C-x >" . scroll-left)
  ("<wheel-left>" . scroll-left)
  ("<wheel-right>" . scroll-right)

  :config
  ;; Available cycle positions for `recenter-top-bottom'.
  (setopt recenter-positions '(top middle bottom))

  (setopt scroll-error-top-bottom t
          ;; Prevent unwanted horizontal scrolling upon navigation.
          scroll-preserve-screen-position t
          scroll-conservatively 10000)

  ;; Add a margin when scrolling vertically (or don't).
  (setq-default scroll-margin 1))

;; Auto-revert buffers


(use-feature! autorevert
  :hook (ceamx-after-init . global-auto-revert-mode)
  :config
  ;; Ensure the non-file-visiting buffers are also auto-reverted as needed.  For
  ;; example, this will cause Dired to refresh a file list when the directory
  ;; contents have changed.
  (setopt global-auto-revert-non-file-buffers t)

  (setopt auto-revert-interval 2))

;; Normalize whitespace and indentation handling


(use-feature! emacs
  :hook ((before-save . delete-trailing-whitespace))

  :config
  (setq-default indent-tabs-mode nil
                tab-width 8)

  (setopt backward-delete-char-untabify-method 'hungry)
  (setopt mode-require-final-newline 'visit-save)
  (setopt sentence-end-double-space t)

  (electric-indent-mode 1))

;; Visualize notable and unusual whitespace


(use-feature! emacs
  :hook ((prog-mode . whitespace-mode))

  :config
  (setq-default indicate-empty-lines nil)

  (setopt whitespace-style
          '(face
            tabs
            tab-mark
            trailing
            missing-newline-at-eof)))

;; Enforce EditorConfig settings

;; - website :: <https://editorconfig.org>


(use-package editorconfig
  :ensure t
  :hook (ceamx-emacs-startup . editorconfig-mode)

  :preface
  ;; via <https://github.com/doomemacs/doomemacs/commit/43870bf8318f6471c4ce5e14565c9f0a3fb6e368>
  (defun +editorconfig-enforce-org-mode-tab-width-h (props)
  "Prevent `editorconfig' from changing `tab-width' in `org-mode'.
A \"tab-width\" of any value other than 8 is an error state in
org-mode, so it must not be changed.

PROPS is as in `editorconfig-after-apply-functions'."
  (when (and (gethash 'indent_size props)
             (derived-mode-p 'org-mode))
    (setq tab-width 8)))

  :config
  (add-hook 'editorconfig-after-apply-functions #'+editorconfig-enforce-org-mode-tab-width-h))

;; ~mwim~: Replace ~beginning-of-line~ and ~end-of-line~ with DWIM alternatives


(use-package mwim
  :ensure t
  :init
  (keymap-global-set "C-a" #'mwim-beginning)
  (keymap-global-set "C-e" #'mwim-end))

;; INPRG Provide a command to intelligently kill words backwardsly

;; - State "INPRG"      from "TODO"       [2024-07-13 Sat 22:02] \\
;;   Needs a fix for compatibility with ~subword-mode~.  See also [[*Don't consider camelCaseWORDs as separate words]]
;; - src :: https://www.reddit.com/r/emacs/comments/bz9rxn/comment/er0bgll/
;; - src :: https://github.com/yantar92/emacs-config/blob/master/config.org#smarter-backward-kill-word


(defun ceamx/backward-kill-word ()
  "Kill the previous word, smartly.
This operation will respect the following rules:

1. If the cursor is at the beginning of line, delete the '\n'.
2. If there is *only* whitespace, delete only to beginning of line.
3. If there is *some* whitespace, delete whitespace and check 4-5.
4. If there are other characters instead of words, delete one only char.
5. If it's a word at point, delete it."
  (interactive)
  (if (bolp)
      ;; 1
      (delete-char -1)
    (if (string-match-p "^[[:space:]]+$"
                        (buffer-substring-no-properties
                         (line-beginning-position) (point)))
        ;; 2
        (delete-horizontal-space)
      (when (thing-at-point 'whitespace)
        ;; 3
        (delete-horizontal-space))

      (if (thing-at-point 'word)
          ;; 5
          (let ((start (car (bounds-of-thing-at-point 'word)))
                (end (point)))
            (if (> end start)
                (delete-region start end)
              (delete-char -1)))
        ;; 4
        (delete-char -1)))))

;; Replace region when inserting text


(delete-selection-mode 1)

;; ~easy-kill~ :package:

;; <https://github.com/leoliu/easy-kill/blob/master/README.rst>

;; #+begin_example
;; w => word
;; s => sexp
;; l => list
;; d => defun
;; D => defun name
;; f => file
;; b => buffer name
;;        ->"-": `default-directory'
;;        ->"+": full path
;;        ->"0": basename
;; #+end_example


(package! easy-kill
  (keymap-global-set "M-w" #'easy-kill)   ; override `kill-ring-save'
  (keymap-global-set "C-M-@" #'easy-mark) ; override `mark-sexp'
  )

;; ~expand-region~: Increase/decrease the selection area


(use-package expand-region
  :ensure t
  :init
  (keymap-global-set "C-=" #'er/expand-region))

;; ~drag-stuff~: drag stuff around in arbitrary directions :package:

;; <https://github.com/rejeep/drag-stuff.el>

;;  This package appears to be abandoned since 2017.  As of <2024-12-27>,
;;  it still works relatively well.  However, there may be some subtle
;;  conflicts with ~org-metaup~ and ~org-metadown~.


(use-package drag-stuff
  :ensure t
  :bind
  (([M-up] . drag-stuff-up)
   ([M-right] . drag-stuff-right)
   ([M-down] . drag-stuff-down)
   ([M-left] . drag-stuff-left)))

;; Visualize and electrify matching character pairs :pairs:

;; See the Info node [[info:emacs#Matching]]



(setopt blink-matching-paren t)
;; Avoid "expression" style, which looks too much like a selected region.
(setopt show-paren-style 'parenthesis)

(setopt electric-pair-preserve-balance t)
(setopt electric-pair-delete-adjacent-pairs t)
(setopt electric-pair-skip-whitespace t)
;; TODO: evaluating...
(setopt electric-pair-open-newline-between-pairs t)

(electric-pair-mode 1)
(show-paren-mode 1)

;; Don't consider camelCaseWORDs as separate words

;; While it can be useful in some contexts, I wish that ~subword-mode~ did not break
;; ~ceamx/backward-kill-word~.  See also [[*Provide a command to intelligently kill
;; words backwardsly]]


(global-subword-mode -1)

;; TODO ~string-inflection~: Commands to cycle through word casing

;; Needs better bindings.


(require 'lib-editor)

(package! string-inflection)

(defvar-keymap ceamx-string-repeat-map
  :repeat t

  "c" #'ceamx/cycle-string-inflection)

(defun ceamx/cycle-string-inflection ()
  "Cycle through `string-inflection' styles appropriate to the major-mode."
  (interactive)
  (pcase major-mode
    (`emacs-lisp-mode (string-inflection-all-cycle))
    (`python-mode (string-inflection-python-style-cycle))
    (`java-mode (string-inflection-java-style-cycle))
    (`elixir-mode (string-inflection-elixir-style-cycle))
    (_ (string-inflection-ruby-style-cycle))))

;; ~ialign~: Interactively ~align-regexp~ :package:

;; <https://github.com/mkcms/interactive-align/blob/master/README.org#usage>


(package! ialign
  (keymap-global-set "C-x l" #'ialign))

;; ~rect~ [builtin]: operate on a buffer rectangularly

;; <https://github.com/abo-abo/hydra/wiki/Rectangle-Operations#rectangle-2>


(use-feature! rect
  :config
  (use-feature! hydra
    :config
    (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                         :color pink
                                         :hint nil
                                         :post (deactivate-mark))
      "
  ^_k_^       _w_ copy      _o_pen       _N_umber-lines            |\\     -,,,--,,_
_h_   _l_     _y_ank        _t_ype       _e_xchange-point          /,`.-'`'   ..  \-;;,_
  ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark      |,4-  ) )_   .;.(  `'-'
^^^^          _u_ndo        _g_ quit     ^ ^                     '---''(./..)-'(_\_)
"
      ("k" rectangle-previous-line)
      ("j" rectangle-next-line)
      ("h" rectangle-backward-char)
      ("l" rectangle-forward-char)
      ("d" kill-rectangle)               ;; C-x r k
      ("y" yank-rectangle)               ;; C-x r y
      ("w" copy-rectangle-as-kill)       ;; C-x r M-w
      ("o" open-rectangle)               ;; C-x r o
      ("t" string-rectangle)             ;; C-x r t
      ("c" clear-rectangle)              ;; C-x r c
      ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
      ("N" rectangle-number-lines)            ;; C-x r N
      ("r" (if (region-active-p)
               (deactivate-mark)
             (rectangle-mark-mode 1)))
      ("u" undo nil)
      ("g" nil))

    (when (fboundp 'hydra-rectangle/body)
      (keymap-global-set "C-x SPC" #'hydra-rectangle/body)
      (keymap-global-set "C-x M-r" #'rectangle-mark-mode))))

;; Line wrapping


(use-feature! emacs
  :hook (((prog-mode text-mode) . auto-fill-mode))

  :config
  (setq-default fill-column 70)
  ;; Disable line soft-wrapping by default.
  (setq-default truncate-lines t)

  (setopt comment-auto-fill-only-comments t))

(use-package unfill
  :ensure t
  :bind ("M-q" . unfill-toggle))

;; Configure secrets lookup with ~auth-source~ and =password-store=

;; - source :: <https://github.com/jwiegley/dot-emacs/blob/9d595c427136e2709dee33271db1a658493265bd/init.org#auth-source-pass>


(use-feature! auth-source
  :demand t
  :config
  ;; Ensure the usage of an encrypted auth credentials file.  It's
  ;; best to list only a single file here to avoid confusion about
  ;; where secrets might be stored.
  (setopt auth-sources (list "~/.authinfo.gpg")))

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

;; ~ceamx-simple~: Simple & common commands


(use-feature! ceamx-simple
  :demand t
  :config
  (keymap-global-set "C-<" #'ceamx-simple/escape-url-dwim)
  (keymap-substitute (current-global-map) #'default-indent-new-line #'ceamx-simple/continue-comment))

;; Linkify URLs and email addresses with ~goto-address~ [builtin]


(autoload 'goto-address-prog-mode "goto-addr")

(add-hook 'prog-mode-hook #'goto-address-prog-mode)

;; ~link-hint~: Activate links in buffer with ~avy~

;; <https://github.com/noctuid/link-hint.el>


(package! link-hint
  (define-keymap :keymap (current-global-map)
    "M-g u" #'link-hint-open-link
    "M-g U" #'link-hint-copy-link))

;; Manage backup files and prevent file-lock clutter


(use-feature! emacs
  :config
  (setopt create-lockfiles nil
          ;; TODO: enable under some conditions e.g. not a project,
          ;; tramp remote file
          make-backup-files nil
          delete-by-moving-to-trash t)

  (when make-backup-files
    (setopt version-control t
            delete-old-versions t
            kept-new-versions 5
            kept-old-versions 5)))

;; Configure finding of files


(use-feature! emacs
  :config
  (setopt find-file-suppress-same-file-warnings t
          find-file-visit-truename t)

  ;; via <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/lisp/doom-editor.el#L78-L89>
  (def-hook! ceamx-find-file-create-paths-h ()
    'find-file-not-found-functions
    "Automatically create missing directories when creating new files."
    (unless (file-remote-p buffer-file-name)
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (and (not (file-directory-p parent-directory))
             (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                               parent-directory))
             (progn (make-directory parent-directory 'parents)
                    t))))))

;; Auto-save file-visiting buffers


(use-feature! emacs
  :config
  (setopt
   ;; Prevent creation of the list of all auto-saved files.
   auto-save-list-file-prefix nil
   ;; Number of input events before autosave
   auto-save-interval 300
   ;; Idle interval for all file-visiting buffers
   auto-save-visited-interval 30
   ;; Idle interval before autosave
   auto-save-timeout 30
   ;; Don't create auto-save "~" files.
   auto-save-default nil)

  ;; Save file-visiting buffers according to the configured timers.
  (auto-save-visited-mode))

;; Provide "Casual" transient menus for complex modes


(package! casual-suite
  (require 'casual-suite)

  (keymap-global-set "C-o" #'casual-editkit-main-tmenu)
  (keymap-set symbol-overlay-map "C-o" #'casual-symbol-overlay-tmenu)

  ;; <https://github.com/kickingvegas/casual-avy>
  ;; M-g M-g
  (keymap-set goto-map "M-g" #'casual-avy-tmenu)

  ;; <https://github.com/kickingvegas/casual-calc>
  (after! calc
    (keymap-set calc-mode-map "C-o" #'casual-calc-tmenu))
  (after! calc-alg
    (keymap-set calc-alg-map "C-o" #'casual-calc-tmenu))

  ;; <https://github.com/kickingvegas/casual-dired>
  (after! dired
    (keymap-set dired-mode-map "C-o" #'casual-dired-tmenu))

  ;; <https://github.com/kickingvegas/casual-info>
  (after! info
    (keymap-set Info-mode-map "C-o" #'casual-info-tmenu))

  ;; <https://github.com/kickingvegas/casual-isearch>
  (after! isearch
    (keymap-set isearch-mode-map "<f2>" #'casual-isearch-tmenu))

  (after! ibuffer
    (keymap-set ibuffer-mode-map "C-o" #'casual-ibuffer-tmenu)
    (keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
    (keymap-set ibuffer-mode-map "s" #'casual-ibuffer-sortby-tmenu))

  (after! re-builder
    (keymap-set reb-mode-map "C-o" #'casual-re-builder-tmenu)
    (keymap-set reb-lisp-mode-map "C-o" #'casual-re-builder-tmenu))

  (after! bookmark
    (keymap-set bookmark-bmenu-mode-map "C-o" #'casual-bookmarks-tmenu))

  (after! org-agenda
    (keymap-set org-agenda-mode-map "C-o" #'casual-agenda-tmenu)))

;; `Info-mode' enchantments


(use-feature! info
  :hook ((Info-mode . hl-line-mode)
         (Info-mode . scroll-lock-mode)))

;; ~helpful~: Provide improved alternatives to the builtin "describe" utilities

;; - Source code :: <https://github.com/Wilfred/helpful>

;; Note that there is a severe but edge-case bug that has gone unfixed
;; for quite a while.  ~helpful~ cannot display documentation for symbols
;; defined in Emacs C source code:

;; <https://github.com/Wilfred/helpful/issues/329>



(use-package helpful
  :ensure t
  ;; Avoid a first-time lag when asking for help, which often happens before an
  ;; idle timer has the chance to run.
  :demand t

  :init
  (define-keymap :keymap help-map
    "c" #'helpful-callable
    "C" #'helpful-command
    "f" #'helpful-function              ; orig: `describe-face'
    "h" #'helpful-at-point
    ;; TODO: consider swapping with the original as a trial?
    "k" #'helpful-key                   ; orig: `describe-key-briefly'
    "o" #'helpful-symbol
    "v" #'helpful-variable

    ;; Parity with the corresponding unmodded keys.
    ;; Primarily for Meow keypad, but also sometimes feels more natural to keep
    ;; holding Ctrl anyway.
    "C-k" #'helpful-key
    "C-o" #'helpful-symbol

    ;; Rebind the originals
    "F" #'describe-face
    "K" #'describe-key-briefly

    ;; Unbind the default binding for "C-h C-h" to allow `which-key' paging.
    "C-h" nil))

;; Rebind some default keybindings in the [C-h] ~help-map~


(define-keymap :keymap help-map
  "l" #'find-library
  ;; I actually prefer the default `man' over `consult-man'.
  "m" #'man                     ; orig: `describe-mode'
  "M" #'describe-mode

  ;; FIXME: no lambda binding
  ;; "t" `("text-props (pt)" . ,(cmd!!
  ;;                              #'describe-text-properties
  ;;                              current-prefix-arg
  ;;                              (point)))

  ;; Unbind the default binding for "C-h C-h" to allow `which-key' paging.
  "C-h" nil)

;; Record some variables' values with ~savehist~ [builtin]


(use-feature! savehist
  :init
  (savehist-mode)

  :config
  (cl-dolist (save '(kill-ring
                     regexp-search-ring
                     search-ring))
    (cl-pushnew save savehist-additional-variables))

  (setopt savehist-autosave-interval 60))

;; Record point position in buffers with ~saveplace~ [builtin]


(use-feature! saveplace
  :init
  (save-place-mode))

;; Record recently-accessed files with ~recentf~ [builtin]


(use-feature! recentf
  :init
  (recentf-mode)

  :config
  (setopt recentf-max-saved-items 50)   ; default => 20
  (setopt recentf-max-menu-items 15)    ; default => 10

  ;; Disable recentf-cleanup on Emacs start, because it can cause
  ;; problems with remote files.
  (setopt recentf-auto-cleanup 'never)

  ;; Exclude internal plumbing files.
  (dolist (path '(ceamx-etc-dir ceamx-var-dir))
    (add-to-list 'recentf-exclude path)))

;; Increase undo history limits

;; Advice from the author of ~undo-fu~:

;; #+begin_quote
;; The default undo limits for emacs are quite low _(0.15mb at time of
;; writing)_ undo-tree for example increases these limits.

;; On modern systems you may wish to use much higher limits.

;; This example sets the limit to 64mb, 1.5x (96mb) for the strong limit
;; and 10x (960mb) for the outer limit.  Emacs uses 100x for the outer
;; limit but this may be too high when using increased limits.
;; #+end_quote

;; via <https://codeberg.org/ideasman42/emacs-undo-fu#undo-limits>


(setopt undo-limit 67108864) ; 64mb.
(setopt undo-strong-limit 100663296) ; 96mb.
(setopt undo-outer-limit 1006632960) ; 960mb.

;; ~undo-fu~: Support optional linear undo/redo

;; - Source code :: <https://codeberg.org/ideasman42/emacs-undo-fu>


(package! undo-fu
  (keymap-global-set "C-z" #'undo-fu-only-undo)
  (keymap-global-set "C-S-z" #'undo-fu-only-redo))

;; ~undo-fu-session~: Record undo/redo steps across Emacs sessions

;; - Source code :: <https://codeberg.org/ideasman42/emacs-undo-fu-session>

;; NOTE: This is *NOT* just for use with ~undo-fu~!  It's an essential
;; enhancement to the builtin Emacs undo system as well.


(defvar undo-fu-session-directory
  (expand-file-name "undo-fu-session" ceamx-var-dir))

(package! undo-fu-session
  (setopt undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (setopt undo-fu-session-ignore-temp-files t)
  (setopt undo-fu-session-ignore-encrypted-files t)

  (setopt undo-fu-session-compression 'zst)

  (undo-fu-session-global-mode))

;; ~vundo~: Visualize the Emacs undo tree

;; - Source code :: <https://github.com/casouri/vundo>


(use-package vundo
  :ensure t
  :defer t
  :defines vundo-unicode-symbols

  :bind
  ("C-x u" . vundo)

  :config
  (setopt vundo-glyph-alist vundo-unicode-symbols))

;; ~dogears~: Return to previously-visited buffer positions

;; - Source code :: <https://github.com/alphapapa/dogears.el>


(package! dogears
  (add-hook 'on-first-buffer-hook #'dogears-mode)

  ;; Also see `ceamx/dogears-dispatch'.
  (define-keymap :keymap (current-global-map)
    ;; TODO: find a new binding maybe
    ;; "M-g d" #'dogears-go
    "M-g M-b" #'dogears-back
    "M-g M-f" #'dogears-forward
    "M-g M-d" #'dogears-list
    "M-g M-D" #'dogears-sidebar)

  ;; Persist `dogears-list' between Emacs sessions.
  ;; via <https://github.com/alphapapa/dogears.el/issues/4>
  (after! savehist
    (when (boundp 'savehist-additional-variables)
      (add-to-list 'savehist-additional-variables #'dogears-list))))

;; TODO: provide a little more context in transient (label for dogears, links maybe...)
(after! (transient dogears)
  (transient-define-prefix ceamx/dogears-dispatch ()
    "Transient menu for `dogears' history navigation commands."
    [["Navigate"
      ("b" "back" dogears-back :transient transient--do-stay)
      ("f" "forward" dogears-forward :transient transient--do-stay)]
     ;; TODO: when quit one of these Find commands, return to transient
     ["Find"
      ("d" "go..." dogears-go)
      ("l" "list" dogears-list)
      ("S" "sidebar" dogears-sidebar)]])

  (defer-until! (fboundp 'ceamx/dogears-dispatch)
    (keymap-global-set "M-g d" #'ceamx/dogears-dispatch)))

;; =init.el=: Window
;; :PROPERTIES:
;; :header-args: :tangle init.el
;; :END:


(require 'ceamx-simple)
(require 'ceamx-window)

;; Define the user option specifying a fallback buffer :buffer:


(defcustom ceamx-fallback-buffer-name "*scratch*"
  "The name of the buffer to fall back to if no other buffers exist.
The buffer will be created if it does not exist."
  :group 'ceamx
  :type '(string))

;; Configure window behavior for help buffers


;; Focus newly-opened help windows.
(setopt help-window-select t)

;; Also focus newly-opened manpages, which still do not follow `display-buffer'
;; rules (as of <2024-03-06>).
(setopt Man-notify-method 'aggressive)

;; ~lentic~: Create decoupled views of the same content :package:


(package! lentic
  (global-lentic-mode))

(with-eval-after-load 'lentic
  (add-to-list 'safe-local-variable-values '(lentic-init . lentic-orgel-org-init)))

;; Disambiguate/uniquify buffer names


(use-feature! emacs
  :config
  (setopt uniquify-buffer-name-style 'forward)
  (setopt uniquify-separator "/")

  ;; Rename after killing uniquified buffer.
  (setopt uniquify-after-kill-buffer-p t)

  ;; Don't muck with special buffers.
  (setopt uniquify-ignore-buffers-re "^\\*"))

;; General buffer display settings :buffer:frame:display_buffer:


(setopt switch-to-buffer-in-dedicated-window 'pop)

;; Ensure interactive buffer switching behaves according to expectations.
(setopt switch-to-buffer-obey-display-actions t)

;; Hide buffer until there's output.
;; Prevents an extra window appearing during init.
(setopt async-shell-command-display-buffer nil)

;; TODO: causes which-key squishing against tiny window maybe?
(setopt fit-window-to-buffer-horizontally t)

;; TODO: this might be a solution to issues with childframes for embark etc.
(setopt fit-frame-to-buffer t)

;; (setopt even-window-sizes nil)
(setopt even-window-sizes 'height-only)
(setopt window-combination-resize t)
(setopt window-sides-vertical nil)
(setopt window-resize-pixelwise t)

(setopt display-buffer-base-action
        '((display-buffer-reuse-window
           display-buffer-in-previous-window)))

;; Declare rules for displaying buffers with ~display-buffer-alist~ :display_buffer:

;; - Source :: <https://github.com/karthink/.emacs.d/blob/6aa2e034ce641af60c317697de786bedc2f43a71/lisp/setup-windows.el>

;; <karthink> has a helpful summary of ~display-buffer~ action functions and
;; alist entries in their Emacs configuration, which I am also including here
;; for my own reference. Note that this list is not necessarily complete.

;; ~display-buffer-action-functions~ are:

;; - ~display-buffer-same-window~ :: Use the selected window
;; - ~display-buffer-reuse-window~ :: Use a window already showing the buffer
;; - ~display-buffer-reuse-mode-window~ :: Use a window with the same major-mode
;; - ~display-buffer-in-previous-window~ :: Use a window that did show the buffer before
;; - ~display-buffer-use-some-window~ :: Use some existing window
;; - ~display-buffer-pop-up-window~ :: Pop up a new window
;; - ~display-buffer-below-selected~ :: Use or pop up a window below the selected one
;; - ~display-buffer-at-bottom~ :: Use or pop up a window at the bottom of the selected frame
;; - ~display-buffer-pop-up-frame~ :: Show the buffer on a new frame
;; - ~display-buffer-in-child-frame~ :: Show the buffer in a child frame
;; - ~display-buffer-no-window~ :: Do not display the buffer and have ~display-buffer~ return nil immediately

;; Action alist entries are:

;; - ~inhibit-same-window~ :: A non-nil value prevents the sam
;;     window from being used for display
;; - ~inhibit-switch-frame~ :: A non-nil value prevents any fram
;;     used for showing the buffer from being raised or selected
;; - ~reusable-frames~ :: The value specifies the set of frames t
;;     search for a window that already displays the buffer.
;;     Possible values are nil (the selected frame), t (any live
;;     frame), visible (any visible frame), 0 (any visible or
;;     iconified frame) or an existing live frame.
;; - ~pop-up-frame-parameters~ :: The value specifies an alist o
;;     frame parameters to give a new frame, if one is created.
;; - ~window-height~ :: The value specifies the desired height of th
;;     window chosen and is either an integer (the total height of
;;     the window), a floating point number (the fraction of its
;;     total height with respect to the total height of the frame's
;;     root window) or a function to be called with one argument -
;;     the chosen window.  The function is supposed to adjust the
;;     height of the window; its return value is ignored.  Suitable
;;     functions are ~shrink-window-if-larger-than-buffer~ and
;;     ~fit-window-to-buffer~.
;; - ~window-width~ :: The value specifies the desired width of th
;;     window chosen and is either an integer (the total width of
;;     the window), a floating point number (the fraction of its
;;     total width with respect to the width of the frame's root
;;     window) or a function to be called with one argument - the
;;     chosen window.  The function is supposed to adjust the width
;;     of the window; its return value is ignored.
;; - ~preserve-size~ :: The value should be either (t . nil) t
;;     preserve the width of the chosen window, (nil . t) to
;;     preserve its height or (t . t) to preserve its height and
;;     width in future changes of the window configuration.
;; - ~window-parameters~ :: The value specifies an alist of windo
;;     parameters to give the chosen window.
;; - ~allow-no-window~ :: A non-nil value means that `display-buffer
;;     may not display the buffer and return nil immediately.


;;     <https://github.com/karthink/.emacs.d/blob/6aa2e034ce641af60c317697de786bedc2f43a71/lisp/setup-windows.el>



(setopt display-buffer-alist
        `(
          ;; (,(rx "*" (or "Agenda Commands" "Org Select") "*")
          ;;   (display-buffer-below-selected
          ;;     display-buffer-in-side-window)
          ;;   (body-function . select-window)
          ;;   (window-parameters . ((mode-line-format . nil))))

          (,ceamx-simple-checkers-buffer-names-regexp
           (display-buffer-in-direction
            display-buffer-in-side-window)
           (window-parameters . ((no-other-window . t))))

          ;; TODO: is there not a simpler way than using `ceamx-simple-buffer-which-mode'?
          ;; e.g. `derived-mode-p' or similar
          ((lambda (buf act) (member (ceamx-simple-buffer-which-mode buf) ceamx-simple-message-modes-list))
           (display-buffer-at-bottom
            display-buffer-in-side-window))

          (,(rx "*" (group (or "Compile-Log" "Messages" "Warnings")) "*")
           (display-buffer-at-bottom
            display-buffer-in-side-window
            display-buffer-in-direction))

          (,(rx "*Backtrace*")
           (display-buffer-in-side-window)
           (window-height . 0.2)
           (side . bottom))))

;; ~popper~: Summon and dismiss "popup" windows :popups:package:

;; - Website :: <https://github.com/karthink/popper>


(package! popper
  (define-keymap :keymap (current-global-map)
    "C-`" #'popper-toggle
    "C-~" #'popper-cycle
    "C-M-`" #'popper-toggle-type)

  (setopt popper-reference-buffers
          (append
           ceamx-simple-help-modes-list
           ceamx-simple-help-buffer-names-list
           ceamx-simple-manual-modes-list
           ceamx-simple-repl-modes-list
           ceamx-simple-repl-buffer-names-list
           ceamx-simple-grep-modes-list
           '(+popper-current-buffer-popup-p)
           '(Custom-mode
             compilation-mode
             messages-buffer-mode)
           (list
            ceamx-simple-checkers-buffer-names-regexp)

           ;; The "Home" tabspace, if enabled, will display the Messages buffer.
           (unless (fboundp 'ceamx-workspace-open-tabspace-after-init-h)
             '("\\*Messages\\*"))

           `(,(rx "Output*" eol)
             ,(rx "*" (or
                       "Async-native-compile-log"
                       "Backtrace"
                       "Compile-Log"
                       "Completions"
                       "compilation"
                       "elpaca-diff"
                       "Shell Command Output"
                       "vc"
                       "Warnings")
               "*")
             "^\\*Embark Export"
             "^Calc:"
             "\\*Async Shell Command\\*"
             ;; ("\\*Async Shell Command\\*" . hide)
             ("\\*Detached Shell Command\\*" . hide))))

  ;; Load as early as possible to catch popups as early as possible.
  (popper-mode)
  (popper-echo-mode))

;; Configure overrides in ~popper-repeat-map~


(after! popper
  (defvar-keymap popper-repeat-map
    :repeat t
    "`" #'popper-cycle
    "~" #'popper-cycle-backwards))

;; Configure ~projectile~ integration


(after! (popper projectile)
  (setopt popper-group-function #'popper-group-by-projectile))

;; Restore previous window configurations with ~winner-mode~ [builtin] :history:


(add-hook 'ceamx-after-init-hook #'winner-mode)

;; =golden-ratio=: Automatically resize windows according to Ancient Wisdom :package:


(package! golden-ratio
  (setopt golden-ratio-auto-scale t)
  (setopt golden-ratio-max-width 100))

;; =ace-window=: Interactively manage windows :package:

;; <https://github.com/abo-abo/ace-window>


(package! ace-window
  ;; Same frame only. While it'd be nice to use the default (global), I really
  ;; dislike that it orders window numbers leads to jarring gaps in window
  ;; numbers in the same frame. For example, frame A might have windows numbered
  ;; 1 and 3 and frame B will have window 2.
  (setopt aw-scope 'frame))

;; =transpose-frame=: Transpose and rotate a frame's windows :package:


(package! transpose-frame)

;; ~ceamx/window-dispatch~: a window-management menu :transient:menu:keybinds:


(transient-define-prefix ceamx/window-dispatch ()
  "Window management transient."
  :transient-suffix 'transient--do-stay
  [["Move"
    ("h" "left" windmove-left)
    ("j" "down" windmove-down)
    ("k" "up" windmove-up )
    ("l" "right" windmove-right)
    ("w" "sel" ace-window)]

   ["Resize"
    ("=" "bal" balance-windows)
    ("+" "bal: area" balance-windows-area)
    ("-" "fit: buffer" fit-window-to-buffer)]

   ["Buffer"
    ("b" "buf" consult-buffer)
    ;; ("f" "ff: p" project-find-file)
    ("f" "file" find-file )
    ("F" "file" find-file-other-window)
    ("g" "grep" consult-ripgrep)]

   ["Swarp"
    ("H" "left" ceamx/window-move-left)
    ("J" "down" ceamx/window-move-down)
    ("K" "up" ceamx/window-move-up)
    ("L" "right" ceamx/window-move-right)
    ""
    ("s" "swap" ace-swap-window)
    ("2" "spl: dn" split-window-below)
    ("3" "spl: rt" split-window-right)
    ("SPC" "swap-or-rotate" ceamx/swap-or-rotate-windows)]

   ["Scroll"
    ;; TODO: allow selecting a window (with infix?) to act upon
    ;; NOTE: These are the correct scroll direction commands, which might
    ;; appear to be reversed when comparing with labels.
    ("." "left" scroll-right)
    ("," "right" scroll-left)
    ("SPC" "down" scroll-up)
    ("DEL" "up" scroll-down)]

   ["Lifecycle"
    ("d" "del (this)" delete-window)
    ("D" "del (select)" ace-delete-window)
    ;; ("D" "del: o" delete-other-windows :transient nil)
    ("u" "undo" winner-undo)
    ("U" "redo" winner-redo)
    ""
    ("0" "del" delete-window)
    ("1" "del other" delete-other-windows)
    ""
    ("S" "[ ] sides" window-toggle-side-windows)
    ("`" "[ ] popups" popper-toggle)
    ""
    ("q" "quit" transient-quit-all)]])

;; =init.el=: Load Features
;; :PROPERTIES:
;; :header-args: :tangle init.el
;; :END:


(require 'init-dashboard)
(require 'init-keys)
(require 'init-abbrevs)

;;;; Completions and Selections

(require 'init-search)
(require 'init-completion)

;;;; Actions

(require 'init-embark)

;; Projects / Files
(require 'init-vcs)
(require 'init-dired)

;;;; Workspaces + activities + contexts

(require 'init-workspace)

;;;; Editing
(require 'ceamx-langs)
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

(require 'lib-prog)

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

;; =init.el=: Keybindings
;; :PROPERTIES:
;; :header-args: :tangle init.el
;; :END:


;;
;;;; Command Bindings

;;;;; Prefix: [C-c]

(define-keymap :keymap (current-global-map)
  "C-c [" #'previous-buffer
  "C-c ]" #'next-buffer
  "C-c `" #'mode-line-other-buffer

  "C-c a" #'org-agenda
  "C-c b" (cons "+[buffer]" #'ceamx-buffer-prefix)
  "C-c c" #'org-capture
  "C-c f" (cons "+[file]" #'ceamx-file-prefix)
  "C-c g" #'magit-dispatch
  "C-c G" #'magit-file-dispatch
  "C-c h" #'consult-history
  "C-c i" (cons "+[insert]" #'ceamx-insert-prefix)
  "C-c k" #'consult-kmacro
  "C-c l" (cons "+[code]" #'ceamx-code-prefix)
  "C-c o" (cons "+[launch]" #'ceamx-launch-prefix)
  "C-c q" (cons "+[session]" #'ceamx-session-prefix)
  "C-c t" (cons "+[toggle]" #'ceamx-toggle-prefix)
  "C-c w" (cons "+[window]" #'ceamx-window-prefix))

;;;;; Prefix: [C-x]

(define-keymap :keymap (current-global-map)

  "C-x k" #'ceamx-simple/kill-buffer      ; orig: `kill-buffer'
  "C-x K" #'kill-buffer

  "C-x n n" #'logos-narrow-dwim
  "C-x o" #'ace-window
  ;; "C-x o" #'ceamx/other-window
  "C-x O" #'ace-window

  "C-x =" #'balance-windows
  "C-x +" #'balance-windows-area
  "C-x ]" #'logos-forward-page-dwim
  "C-x [" #'logos-backward-page-dwim
  "C-x SPC" #'hydra-rectangle/body

  "C-x C-b" #'ibuffer
  "C-x C-n" #'next-buffer
  "C-x C-p" #'previous-buffer

  "C-x M-r" #'rectangle-mark-mode

  "C-x <up>" #'enlarge-window           ; also: C-x ^
  "C-x <down>" #'shrink-window
  "C-x <left>" #'shrink-window-horizontally
  "C-x <right>" #'enlarge-window-horizontally)

;;;;; Modifier: [C-]

;;;;; Modifier: [M-]

(define-keymap :keymap (current-global-map)
  "M-]" #'logos-forward-page-dwim
  "M-[" #'logos-backward-page-dwim

  "M-f" #'forward-word
  "M-F" #'forward-symbol
  "M-j" #'avy-goto-char-timer
  "M-Q" #'repunctuate-sentences
  "M-w" #'easy-kill

  "M-DEL" #'ceamx/backward-kill-word
  )

(after! (avy lispy)
  ;; Prevent conflict with newly-added M-j binding.
  (keymap-set lispy-mode-map "M-J" #'lispy-join))

;;;;; [C-c b] :: Buffer

(define-keymap :keymap ceamx-buffer-prefix
  "b" #'consult-buffer
  "k" #'ceamx-simple/kill-buffer)

;;;;; [C-c f] :: File

(define-keymap :keymap ceamx-file-prefix
  ;; TODO
  ;; "y" #'+yank-this-file-name

  "c" '("copy..." . ceamx-simple/copy-current-file)
  "d" '("delete" . ceamx-simple/delete-current-file)
  "f" #'find-file
  "F" #'find-file-other-window
  "r" '("rename/move..." . ceamx-simple/move-current-file)
  "s" #'save-buffer
  "S" '("save as..." . write-file)
  "U" #'ceamx-simple/sudo-find-file

  "C-d" '("diff with..." . ceamx-simple/diff-with-file))

;;;;; [C-c i] :: Insert

(define-keymap :keymap ceamx-insert-prefix
  "d" #'ceamx-simple/insert-date
  ;; "h" #'i-ching-insert-hexagram
  "L" #'spdx-insert-spdx
  "s" #'yas-insert-snippet
  "u" #'uuidgen

  "U" (cons "uuid" (define-prefix-command 'ceamx-insert-uuid-prefix))
  "U 1" #'uuidgen-1
  "U 3" #'uuidgen-3
  "U 4" #'uuidgen-4
  "U 5" #'uuidgen-5)

;;;;; [C-c l] :: Code

(define-keymap :keymap ceamx-code-prefix
  "a" #'eglot-code-actions
  "d" #'xref-find-definitions
  "j" #'ceamx-prog-dumb-jump-dispatch/body
  "o" #'consult-eglot-symbols
  "r" #'eglot-rename
  )

;;;;; [C-c n] :: Note

(define-keymap :keymap ceamx-note-prefix
  "n" #'denote

  "c" #'denote-region                   ; "contents"
  "C" #'denote-type
  "s" #'denote-subdirectory
  "t" #'denote-template
  "z" #'denote-signature                ; "zettelkasten"

  "i" #'denote-link                     ; "insert link"
  "I" #'denote-add-links
  "b" #'denote-backlinks

  "f" (cons "find..." (define-prefix-command 'ceamx-find-notes-prefix))
  "f f" #'denote-find-link
  "f b" #'denote-find-backlink

  "r" #'denote-rename-file
  "R" #'denote-rename-file-using-front-matter)


;;;;; [C-c o] :: Launch

(define-keymap :keymap ceamx-launch-prefix
  "a" #'org-agenda
  "b" #'eww
  "c" #'org-capture
  "f" #'elfeed
  "s" #'scratch-buffer
  "t" #'eat
  "W" #'ceamx/eww-wiki)

;;;;; [C-c q] :: Session

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
  "Q" #'kill-emacs)

;;;;; [C-c t] :: Toggle

(define-keymap :keymap ceamx-toggle-prefix
  "f" #'flycheck-mode
  "k" #'keycast-mode-line-mode
  "l" #'display-line-numbers-mode
  "M" #'menu-bar-mode
  "T" #'tab-bar-mode
  "w" #'window-toggle-side-windows
  "W" #'toggle-window-dedicated
  "z" #'logos-focus-mode)

;;;;; Window

(define-keymap :keymap window-prefix-map
  "w" #'ace-window

  "d" #'ace-delete-window
  "p" #'popper-toggle
  "P" #'popper-toggle-type
  "u" #'winner-undo

  "h" #'windmove-left
  "H" #'ceamx/window-move-left
  "j" #'windmove-down
  "J" #'ceamx/window-move-down
  "k" #'windmove-up
  "K" #'ceamx/window-move-up
  "l" #'windmove-right
  "L" #'ceamx/window-move-right

  "SPC" #'transpose-frame
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

(defvar-keymap ceamx-window-repeat-map
  :repeat t

  "o" #'ace-window
  "SPC" #'transpose-frame
  "<" #'flip-frame
  ">" #'flop-frame
  "[" #'rotate-frame-clockwise
  "]" #'rotate-frame-anticlockwise

  "RET" #'repeat-exit
  "ESC" #'repeat-exit)

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
