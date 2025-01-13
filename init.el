;;; init.el --- Ceamx -*- lexical-binding: t; no-byte-compile: t; -*-

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


(defcustom ceamx-load-custom-file nil
  "Whether to load the user `custom-file' (custom.el)."
  :group 'ceamx
  :type '(boolean))

;; Configure ~custom-file~ location


(setq custom-file (if ceamx-load-custom-file
                      (locate-user-emacs-file "custom.el")
                    (make-temp-file "ceamx-custom-")))

;; Enable/disable some commands that are disabled/enabled by default


;; Enable these commands
(dolist (cmd '(downcase-region
               list-timers
               narrow-to-page
               narrow-to-region
               upcase-region))
  (put cmd 'disabled nil))

;; Disable these commands
(dolist (cmd '(diary iconify-frame overwrite-mode))
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

;; Add the =site-lisp= directory to ~load-path~


(add-to-list 'load-path ceamx-site-lisp-dir)
(prependq! load-path (ceamx-subdirs ceamx-site-lisp-dir))

;; =site-lisp/on=: Define additional Emacs event hooks


(require 'on)

;; Elpaca


(defvar elpaca-directory (expand-file-name "elpaca/" ceamx-packages-dir))

;; Avoid aggressive GitHub API rate limiting.
(defvar elpaca-queue-limit 10)



;; The installation code only needs to be changed when the Elpaca warns
;; about an installer version mismatch.

;; This should be copied verbatim from the Elpaca documentation, with the
;; definition for ~elpaca-directory~ removed.


(defvar elpaca-installer-version 0.8)
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

;; Install the latest version of ~use-package~


(elpaca use-package)

;; ~elpaca-use-package~: integrate ~elpaca~ and ~use-package~


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

;; ~blackout~: adjust mode-line lighters :modeline:

;; - Use-Package keyword :: =:blackout=


(use-package blackout
  :ensure (:wait t)
  :demand t)

;; Install and configure =setup.el=


(elpaca setup
  (require 'setup))

(elpaca-wait)

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

;; ~gcmh~: manage running garbage collection on idle :package:perf:

;; - Website :: <https://akrl.sdf.org/>
;; - Code :: <https://gitlab.com/koral/gcmh>

;; During normal use, the GC threshold will be set to a high value.
;; When idle, GC will be triggered with a low threshold.


(package! gcmh
  (blackout 'gcmh-mode)
  (add-hook 'ceamx-emacs-startup-hook #'gcmh-mode))

;; Install utility libraries


;; FIXME: remove or alias (`##' is very difficult to search for)
(use-package llama) ;  `##' lambda shorthand =>
                                        ;  <https://git.sr.ht/~tarsius/llama>

(use-package f)

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

;; Configure cursor appearance


(package! cursory
  (require 'cursory)

  (def-hook! ceamx-init-theme-cursory-h ()
    'ceamx-after-init-hook
    "Enable `cursory-mode' and load the latest preset."
    (cursory-mode 1)
    (cursory-set-preset (or (cursory-restore-latest-preset) 'box)))

  (setopt cursory-latest-state-file
          (expand-file-name "cursory-latest-state.eld" ceamx-var-dir))

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
             :blink-cursor-delay 0.2))))

;; Customize the Customization buffers and menus


(setopt custom-theme-allow-multiple-selections nil)

(setopt custom-unlispify-menu-entries nil)
(setopt custom-unlispify-tag-names nil)
(setopt custom-unlispify-remove-prefixes nil)

(add-hook 'Custom-mode-hook #'custom-toggle-hide-all-widgets nil t)

;; =grid=: textual data table presentation

;; - Source :: [[https://github.com/ichernyshovvv/grid.el][ichernyshovvv/grid.el]]
;; - Retrieved :: [2024-06-07 Fri 11:45]

;; #+begin_quote
;; This library allows you to put text data into boxes and align them horizontally,
;; applying margin, padding, borders.
;; #+end_quote


(package! (grid :host github :repo "ichernyshovvv/grid.el"))

;; =hydra=

;; - Documentation :: <https://github.com/jerrypnz/major-mode-hydra.el/#pretty-hydra>


(package! hydra)
(package! pretty-hydra)

;; =symbol-overlay= :: highlight symbols with keymap-enabled overlays


(package! symbol-overlay)

;; =transient=


(package! transient
  (require 'transient))

(after! transient
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

(defcustom ceamx-ui-theme-light 'modus-operandi
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

  (setopt modus-themes-italic-constructs t
          modus-themes-bold-constructs t
          modus-themes-mixed-fonts t
          modus-themes-variable-pitch-ui nil)
  (setopt modus-themes-to-toggle '(modus-operandi modus-vivendi))
  (setopt modus-themes-disable-other-themes t)
  (setopt modus-themes-headings
          '((agenda-structure . (variable-pitch light 2.2))
            (agenda-date . (variable-pitch regular 1.3))
            (t . (regular 1.1)))))

;; Ef-Themes :package:

;; - Website :: <https://protesilaos.com/emacs/ef-themes>


(defvar ceamx-font-headings-style-alist)

(package! ef-themes
  (require 'ef-themes)

  (setopt ef-themes-to-toggle '(ef-night ef-frost)
          ef-themes-mixed-fonts t
          ef-themes-variable-pitch-ui nil))

;; Theme Phasing Schedule


(defcustom ceamx-ui-theme-circadian-interval nil
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


(setopt ceamx-ui-theme-light 'modus-operandi)
(setopt ceamx-ui-theme-dark 'modus-vivendi)

(if (eq 'solar ceamx-ui-theme-circadian-interval)
    (after! circadian (add-hook 'ceamx-after-init-hook #'circadian-setup))
  (add-hook 'ceamx-after-init-hook
            (lambda ()
              (if (ceamx-ui-desktop-dark-theme-p)
                  (ceamx-ui/load-dark-theme)
                (ceamx-ui/load-light-theme)))))

;; =avy= :: can do anything

;; + Package :: <https://github.com/abo-abo/avy>
;; + Article :: <https://karthinks.com/software/avy-can-do-anything/>


(package! avy
  (defer! 3
    (require 'avy)))

(after! avy
  (setopt avy-style 'at-full)
  (setopt avy-all-windows t)
  (setopt avy-case-fold-search t)

  ;; Prevent conflicts with themes.
  (setopt avy-background nil)

  ;; Anything lower feels unusable.
  (setopt avy-timeout-seconds 0.25))

;; Line highlighting


(add-hook 'prog-mode-hook #'hl-line-mode)

(after! hl-line
  ;; Disable line highlight in unfocused windows.
  (setopt hl-line-sticky-flag nil))



;; Improve line-highlighting for major-modes orientated around line selection:


(package! lin
  (add-hook 'ceamx-after-init-hook #'lin-global-mode))



;; Pulse current line after function invocations:


(package! pulsar
  (add-hook 'ceamx-after-init-hook #'pulsar-global-mode)
  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line))

(after! pulsar
  (setopt pulsar-pulse t
          pulsar-delay 0.055
          pulsar-iterations 10)
  (setopt pulsar-face 'pulsar-magenta
          pulsar-highlight-face 'pulsar-cyan)

  (dolist (fn '(pulsar-pulse-line-red
                pulsar-recenter-top
                pulsar-reveal-entry))
    (add-hook 'next-error-hook #'fn)))

;; Window highlighting


(setopt highlight-nonselected-windows nil)

;; Hide frame decorations


(unless (ceamx-host-macos-p)
  (add-to-list 'default-frame-alist '(undecorated . t)))

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

;; ~spacious-padding~: a comfortable layout density


(package! spacious-padding
  (add-hook 'ceamx-after-init-hook #'spacious-padding-mode))

(after! spacious-padding
  (setopt spacious-padding-widths
          '( :internal-border-width 30
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

;; =olivetti= :: "distraction-free" editing

;; - Package :: <https://github.com/rnkn/olivetti>


(package! olivetti
  (keymap-set ctl-x-x-map "o" #'olivetti-mode))

(after! olivetti
  (setopt olivetti-body-width 0.7
          olivetti-minimum-body-width 80
          olivetti-recall-visual-line-mode-entry-state t))

;; =logos= :: a simple focus mode with page breaks or outlines :present:


(package! logos
  (after! logos
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

    (def-hook! ceamx-essentials-logos-recenter-top-h ()
      '(logos-page-motion-hook)
      "Place point at the top when changing pages in non-`prog-mode' modes."
      (unless (derived-mode-p 'prog-mode)
        ;; NOTE: '0' value will recenter at the absolute top.
        (recenter 1)))))

;; =moc= :: "Master of Ceremonies" presentation utilities :present:

;; + Package :: <https://github.com/positron-solutions/moc/>


(package! (moc :host github :repo "positron-solutions/moc"))

;; Allow restoring deleted frames


(undelete-frame-mode 1)

;; Menu Bar :menubar:

;; Disable the menu bar by default:


(menu-bar-mode -1)

;; Tab Bar :tabs:

;; Enable the tab bar:


(tab-bar-mode 1)

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

;; ~fontaine~ :: pre-configure font presets

;; <https://protesilaos.com/emacs/fontaine>

;; TIP: You can test out alterations quickly with, for example:
;;      (internal-set-lisp-face-attribute 'default :weight 'semilight)


(package! fontaine
  (when (display-graphic-p)
    (def-hook! ceamx-init-theme-activate-fontaine-h ()
      'ceamx-after-init-hook
      "Activate `fontaine-mode' with the last-saved preset.
If there is no previous preset state to load, fall back to the
\"regular\" preset."
      (fontaine-mode)
      (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))))

(after! fontaine
  (setopt fontaine-latest-state-file
          (expand-file-name "fontaine-latest-state.eld" ceamx-var-dir))

  ;; For some reason I do not yet understand, according to some
  ;; hearsay, font sizes best scale in multiples of 3-point
  ;; increments.
  (setopt fontaine-presets
          `((tiny
             :bold-weight medium
             :default-height ,(pcase (system-name)
                               (_ 78))
             :default-weight ,(pcase (system-name)
                               (_ 'semilight)))

            (small
             :bold-weight semibold
             :default-height ,(pcase (system-name)
                               (_ 90))
             :default-weight ,(pcase (system-name)
                               (_ 'regular)))

            (regular
             :bold-weight semibold)

            (medium
             :default-height ,(pcase (system-name)
                               ("boschic" 124)
                               (_ 120)))

            (large
             :default-height ,(pcase (system-name)
                               (_ 144))
             :default-weight semilight)

            (xlarge
             :default-height ,(pcase (system-name)
                               (_ 156)))

            (big-mclarge-huge
             :default-weight semilight
             :default-height ,(pcase (system-name)
                               (_ 180))
             :bold-weight extrabold)

            (t
             :default-family "Iosevka Comfy"
             :default-weight regular
             :default-height ,(pcase (system-name)
                               ("tuuvok" 102)
                               (_ 105))

             :fixed-pitch-family "Iosevka Comfy"
             :fixed-pitch-weight nil
             :fixed-pitch-height 1.0

             :fixed-pitch-serif-family nil
             :fixed-pitch-serif-weight nil
             :fixed-pitch-serif-height 1.0

             :variable-pitch-family "Iosevka Comfy Motion Duo"
             :variable-pitch-weight nil
             :variable-pitch-height 1.0

             :mode-line-active-family nil
             :mode-line-active-weight nil
             :mode-line-active-height 0.9

             :mode-line-inactive-family nil
             :mode-line-inactive-weight nil
             :mode-line-inactive-height 0.9

             :header-line-family nil
             :header-line-weight nil
             :header-line-height 0.9

             :line-number-family nil
             :line-number-weight nil
             :line-number-height 0.9

             :tab-bar-family nil
             :tab-bar-weight nil
             :tab-bar-height 1.0

             :bold-family nil
             :bold-weight bold

             :italic-family nil
             :italic-weight nil
             :italic-slant italic

             :line-spacing nil))))

;; ~ligature.el~ :: improved ligature support

;; + Package :: <https://github.com/mickeynp/ligature.el>

;; A better implementation of ligature support than the builtin
;; ~prettify-symbols-mode~.

;; <https://old.reddit.com/r/emacs/comments/keji66/what_is_bad_about_prettifysymbolsmode/>


(package! ligature
  (when (display-graphic-p)
    (after! fontaine
      (global-ligature-mode 1))))

(after! ligature
  ;; Enable all Iosevka ligatures in programming modes.
  ;; <https://github.com/mickeynp/ligature.el/wiki#iosevka>
  (ligature-set-ligatures
   'prog-mode
   '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->"
     "<---->" "<!--" "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">="
     "<=>" "<==>" "<===>" "<====>" "<!---" "<~~" "<~" "~>" "~~>"
     "::" ":::" "==" "!=" "===" "!==" ":=" ":-" ":+" "<*" "<*>"
     "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++")))

;; ~show-font~ :: a tool to preview fonts

;; + Package :: <https://protesilaos.com/emacs/show-font>


(when (display-graphic-p)
  (package! show-font)

  (after! show-font
    (setopt show-font-pangram 'ceamx)
    (setopt show-font-character-sample
            "
ABCDEFGHIJKLMNOPQRSTUVWXYZ
abcdefghijklmnopqrstuvwxyz
0123456789   !@#$¢%^&*~|
`'\"‘’“”.,;:  ()[]{}—-_+=<>

()[]{}<>«»‹› 6bB8&0ODdoa 1tiIlL|\/
!ij c¢ 5$Ss 7Z2z 9gqp nmMNNMW uvvwWuuw
x×X .,·°;:¡!¿?`'‘’   ÄAÃÀ TODO
")))

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

;; =init.el=: Window
;; :PROPERTIES:
;; :header-args: :tangle init.el
;; :END:


(require 'ceamx-simple)
(require 'ceamx-window)

;; Define the user option specifying a fallback buffer


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

;; =lentic=: Create decoupled views of the same content


(package! lentic
  (global-lentic-mode))

(with-eval-after-load 'lentic
  (add-to-list 'safe-local-variable-values '(lentic-init . lentic-orgel-org-init)))

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


(require 'ceamx-init-essentials)
(require 'ceamx-init-completion)
(require 'init-embark)
(require 'ceamx-init-search)
(require 'ceamx-init-dired)
(require 'init-workspace)
(require 'ceamx-init-vcs)
(require 'ceamx-init-langs)
(require 'init-writing)
(require 'init-templates)

;;;; Outlines & Memex

(require 'init-notes)
(require 'init-outline)
(require 'ceamx-init-org)

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

;; TODO ~hippie-expand~

;; + [ ] Adds noticeable lag to startup


(setopt hippie-expand-verbose t
        hippie-expand-dabbrev-skip-space t)

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
  "C-c b" (cons "[buffer]" #'ceamx-buffer-prefix)
  "C-c c" #'org-capture
  "C-c f" (cons "[file]" #'ceamx-file-prefix)
  "C-c g" #'magit-dispatch
  "C-c G" #'magit-file-dispatch
  "C-c h" #'consult-history
  "C-c i" (cons "[insert]" #'ceamx-insert-prefix)
  "C-c k" #'consult-kmacro
  "C-c l" (cons "[code]" #'ceamx-code-prefix)
  "C-c o" (cons "[launch]" #'ceamx-launch-prefix)
  "C-c q" (cons "[session]" #'ceamx-session-prefix)
  "C-c t" (cons "[toggle]" #'ceamx-toggle-prefix)
  "C-c w" (cons "[window]" #'ceamx-window-prefix))

;;;;; Prefix: [C-x]

(define-keymap :keymap (current-global-map)

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

;;;;; Modifier: [M-]

(define-keymap :keymap (current-global-map)
  "M-]" #'logos-forward-page-dwim
  "M-[" #'logos-backward-page-dwim
  "M-j" #'avy-goto-char-timer
  "M-w" #'easy-kill)

(after! (avy lispy)
  ;; Prevent conflict with newly-added M-j binding.
  (keymap-set lispy-mode-map "M-J" #'lispy-join))

;;;;; [C-c b] :: Buffer

(define-keymap :keymap ceamx-buffer-prefix
  "b" #'consult-buffer
  "k" #'ceamx-simple/kill-current-buffer)

;;;;; [C-c f] :: File

(define-keymap :keymap ceamx-file-prefix
  ;; TODO
  ;; "y" #'+yank-this-file-name

  "c" '("copy..." . ceamx-simple/copy-current-file)
  "d" '("delete" . ceamx-simple/delete-current-file)
  "f" #'find-file
  "F" #'find-file-other-window
  "r" '("move..." . ceamx-simple/move-current-file)
  "s" #'save-buffer
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
  "U" #'winner-redo

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

;; Re-enable theme


;; FIXME: wrong num args
;; (ceamx-ui-re-enable-theme-in-frame)
