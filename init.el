;;; init.el --- Ceamx: Experimental Init -*- lexical-binding: t;

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: local

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

;; Experimental single-file initialization.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'map)

;;;;; Core constants
(require 'ceamx-paths)

;;;;; Core functions and macros
(require 'ceamx-lib)

;;;; Basic configuration

(setq-default user-full-name "Chris Montgomery"
              user-mail-address "chmont@protonmail.com")

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;; Increase number of messages saved in log.
(setq message-log-max 10000)

;; Unbind `suspend-frame'.
(global-unset-key (kbd "C-x C-z"))

;; "A second, case-insensitive pass over `auto-mode-alist' is time wasted."
(setopt auto-mode-case-fold nil)

;; Prevent Emacs from pinging domain names unexpectedly.
(setopt ffap-machine-p-known 'reject)

;;;; Add local packages to load-path

(add-to-list 'load-path ceamx-site-lisp-dir)
(prependq! load-path (ceamx-subdirs ceamx-site-lisp-dir))

;;;; Initialize the Ceamx options

(defgroup ceamx nil
  "User-configurable options for Ceamx."
  :group 'emacs)

(defcustom ceamx-load-custom-file nil
  "Whether to load the user `custom-file' (custom.el)."
  :group 'ceamx
  :type '(boolean))

;;;; Define additional Emacs event hooks

(require 'on)

;;;; Package manager

;;;;; Pretend file-visiting-buffers in the package directory are read-only

;; Define a read-only directory class
(dir-locals-set-class-variables
  'read-only
  '((nil . ((buffer-read-only . t)))))

(dir-locals-set-directory-class (file-truename ceamx-packages-dir) 'read-only)

;;;;; Install Elpaca

(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" ceamx-packages-dir))

;; The installation code only needs to be changed when the Elpaca warns about an
;; installer version mismatch.  This should be copied verbatim from the Elpaca
;; documentation sans the above definitions for `elpaca-installer-version' and
;; `elpaca-directory'.

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

;;;;; Elpaca + `use-package' integration

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; Prevent race conditions on the definition of storage paths.  Also,
;; `use-package' must be loaded for byte-compilation checks.
(elpaca-wait)

(setopt use-package-always-ensure t)
(setopt use-package-expand-minimally t)

(when (bound-and-true-p init-file-debug)
  (require 'use-package)
  (setopt use-package-expand-minimally nil)
  (setopt use-package-verbose t)
  (setopt use-package-compute-statistics t))

;;;; Essential early packages

(use-package no-littering
  :demand t
  :wait t

  :preface
  (setq no-littering-etc-directory ceamx-etc-dir)
  (setq no-littering-var-directory ceamx-var-dir))

(use-package blackout
  :demand t
  :wait t)

(use-package gcmh
  :blackout t
  :hook ceamx-emacs-startup-hook)

(use-package llama)

(use-package f)

;;;; Initialize Ceamx prefix commands and their keymaps

(keymap-global-set "C-c q"
  (cons "Session"
    (define-prefix-command 'ceamx-session-prefix 'ceamx-session-prefix-map)))

(keymap-global-set "C-c t"
  (cons "Toggle"
    (define-prefix-command 'ceamx-toggle-prefix 'ceamx-toggle-prefix-map)))


;;;; Environment Integration

(use-package exec-path-from-shell
  :demand t
  :init
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH" "LSP_USE_PLISTS"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package inheritenv
  :demand t
  :after exec-path-from-shell)

(use-package with-editor
  :init
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

(use-package envrc
  :after exec-path-from-shell
  :init (envrc-global-mode)
  :wait t)

(use-feature! tramp
  :config
  (setopt tramp-default-method "ssh")
  (setopt tramp-default-remote-shell "/bin/bash")
  (setopt tramp-connection-timeout (* 60 10))
  ;; Do not auto-save remote files. Note the reversed logic.
  (setopt remote-file-name-inhibit-auto-save t)
  (setopt remote-file-name-inhibit-auto-save-visited t)
  ;; Avoid expensive operations on remote files.
  (setopt remote-file-name-inhibit-delete-by-moving-to-trash t)

  (dolist (path '("~/.local/bin"
                   "~/.nix-profile/bin"
                   "~/.local/state/nix/profiles/profile/bin/"
                   "/nix/var/nix/profiles/default/bin"
                   "/run/current-system/sw/bin"))
    (add-to-list 'tramp-remote-path path)))

;;;; Simple improvements

(use-feature! delsel
  :hook (ceamx-after-init . delete-selection-mode))


;;;; Secrets

;; Configure secrets lookup with ~auth-source~ and the Unix password store

;; Ensure secrets and auth credentials are not stored in plaintext (the default).
;;
;; It's best to list only a single file here to avoid confusion about where
;; secrets might be stored.
(setopt auth-sources (list "~/.authinfo.gpg"))

(use-feature! auth-source-pass
  :init
  (auth-source-pass-enable))

;; Use Emacs for pinentry.
(use-feature! epg
  :defer 2
  :config
  (setopt epg-pinentry-mode 'loopback))

;;;; Appearance

(let ((mono-spaced-font "Monospace")
       (proportionately-spaced-font "Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 100)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

(defconst ceamx-ui-gsettings-ui-namespace "org.gnome.desktop.interface")

(defcustom ceamx-ui-theme-light 'modus-operandi-tinted
  "The default light theme."
  :group 'ceamx
  :type 'symbol)

(defcustom ceamx-ui-theme-dark 'modus-vivendi
  "The default dark theme."
  :group 'ceamx
  :type 'symbol)

(defcustom ceamx-font-height-multiplier 1.0
  "Multiplier for display font size.
Intended for use as a per-system (or, ideally, per-display)
accommodation for varying pixel densities."
  :group 'ceamx
  :type '(float))

(defun ceamx-font-height (number &optional multiplier)
  "Return a numeric font height based on NUMBER multiplied by MULTIPLIER.
NUMBER should be a whole number. MULTIPLIER should be a float.

If MULTIPLIER is nil, the value of `ceamx-font-height-multiplier'
will be used as default."
  (truncate (* number (or multiplier ceamx-font-height-multiplier))))

(defun ceamx-ui-re-enable-theme-in-frame (_frame)
  "Re-enable active theme, if any, upon FRAME creation.
Add this to `after-make-frame-functions' so that new frames do
not retain the generic background set by the function
`ceamx-ui-theme-no-bright-flash'."
  (when-let ((theme (car custom-enabled-themes)))
    (enable-theme theme)))


(defun ceamx-ui-gsettings-theme ()
  "Get the currently-active GNOME/GTK color scheme."
  (shell-command-to-string (format "gsettings get %s color-scheme"
                         ceamx-ui-gsettings-ui-namespace)))

(defun ceamx-ui-gsettings-dark-theme-p ()
  "Whether GNOME/GTK are using a theme with a dark color scheme."
  (string-match-p "dark" (ceamx-ui-gsettings-theme)))

(defun ceamx-ui/gsettings-set-theme (theme)
  "Set the GNOME/GTK theme to THEME."
  ;; FIXME: prompt with completion
  (interactive "s")
  (let* ((namespace ceamx-ui-gsettings-ui-namespace)
         (value (pcase theme
                  ((rx (optional "prefer-") "dark")
                   "prefer-dark")
                  ((rx (optional "prefer-") "light")
                   "prefer-light")
                  (_ "prefer-dark")))
         (cmd (format "gsettings set %s color-scheme %s" namespace value)))
    (shell-command cmd)))

(defun ceamx-ui/gsettings-dark-theme ()
  "Enable the dark GNOME/GTK theme."
  (interactive)
  (ceamx-ui/gsettings-set-theme "dark"))

(defun ceamx-ui/gsettings-light-theme ()
  "Enable the light GNOME/GTK theme."
  (interactive)
  (ceamx-ui/gsettings-set-theme "light"))


(defun ceamx-ui-desktop-dark-theme-p ()
  "Predicate whether a desktop environment is displaying a dark appearance."
  (or (ceamx-ui-gsettings-dark-theme-p)))

;; ~ceamx-ui-load-theme~: function to cleanly load a theme

;; Similar to the theme-family-specific ~modus-themes-load-theme~.


(defun ceamx-ui-load-theme (theme)
  "Load THEME after resetting any previously-loaded themes."
  (mapc #'disable-theme (remq theme custom-enabled-themes))
  (load-theme theme :no-confirm))

;; Commands to load a preferred light or dark Emacs theme


(defun ceamx-ui/load-dark-theme ()
  "Load a random dark theme."
  (interactive)
  (pcase ceamx-ui-theme-circadian-interval
    ('buffet
     (+theme-buffet--load-random-from-periods
      ceamx-ui-theme-buffet-dark-periods))
    (_
     (load-theme ceamx-ui-theme-dark :no-confirm))))

(defun ceamx-ui/load-light-theme ()
  "Load a random light theme."
  (interactive)
  (pcase ceamx-ui-theme-circadian-interval
    ('buffet
     (+theme-buffet--load-random-from-periods
      ceamx-ui-theme-buffet-light-periods))
    (_
     (load-theme ceamx-ui-theme-light :no-confirm))))

;; Commands to globally set a preferred light or dark theme


(defun ceamx-ui/light ()
  "Activate a light theme globally."
  (interactive)
  (ceamx-ui/gsettings-light-theme)
  ;;(ceamx-ui-kitty-set-theme "light")
  (ceamx-ui/load-light-theme))

(defun ceamx-ui/dark ()
  "Activate a dark theme globally."
  (interactive)
  (ceamx-ui/gsettings-dark-theme)
  ;;(ceamx-ui-kitty-set-theme "dark")
  (ceamx-ui/load-dark-theme))

;;;;; Cursor

(use-package cursory
  :ensure t
  :demand t
  :if (display-graphic-p)

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

;;;;; Customize the Customization interfaces

(setopt custom-theme-allow-multiple-selections nil)
(setopt custom-unlispify-menu-entries nil)
(setopt custom-unlispify-tag-names nil)
(setopt custom-unlispify-remove-prefixes nil)

(add-hook 'Custom-mode-hook #'custom-toggle-hide-all-widgets nil t)

;;;;; Theme

;; Consider all themes "safe"
(setopt custom-safe-themes t)

;;;;;; Modus Themes

(use-package modus-themes
  :ensure t

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

(setopt ceamx-ui-theme-light 'modus-operandi-tinted)
(setopt ceamx-ui-theme-dark 'modus-vivendi)

;;;;;; Ef-Themes

(use-package ef-themes
  :config
  (setopt ef-themes-to-toggle '(ef-night ef-frost)
          ef-themes-mixed-fonts t
          ef-themes-variable-pitch-ui nil))

;;;;;; Load a default theme

;; Configure some user options dependent on the loaded packages:

(setopt ceamx-ui-theme-light 'modus-operandi-tinted)
(setopt ceamx-ui-theme-dark 'modus-vivendi)


;;;;; Common libraries and other packages

(use-package (grid :host github :repo "ichernyshovvv/grid.el"))

(use-package hydra)

(use-package pretty-hydra)

(use-feature! transient
  :init
  ;; Restore the default location, overriding `no-littering'.  I consider these
  ;; values configuration to be exposed, not state to be hidden.  See
  ;; `transient-save-values' and related.
  (setopt transient-values-file (locate-user-emacs-file "transient/values.el"))
  :config
  (keymap-set transient-map "<escape>" #'transient-quit-one))

(use-package magit-section)

(use-feature! hl-line
  :hook (prog-mode package-menu-mode))

(use-package pulsar
  :init
  (pulsar-global-mode 1)

  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line))

(use-package nerd-icons
  :demand t
  :init
  (setopt nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package svg-lib)

(use-package page-break-lines
  :init
  (global-page-break-lines-mode))

(use-package rainbow-mode)

;;;;; Density

(use-package spacious-padding
  :demand t
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

;;;;; Menu Bar

(keymap-set ceamx-toggle-map "M" #'menu-bar-mode)

(menu-bar-mode -1)

;;;;; Tab Bar

(keymap-set ceamx-toggle-map "T" #'tab-bar-mode)

(use-feature! tab-bar
  :init
  (tab-bar-mode 1)

  :config
  (setopt tab-bar-auto-width t
    tab-bar-auto-width-max '((80) 10)))


;;;;; Modeline

(line-number-mode 1)
(column-number-mode 1)

(setopt display-time-24hr-format t)

(use-package keycast
  :init
  (keymap-set ceamx-toggle-prefix-map "k" #'keycast-mode-line-mode)

  :config
  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p mouse-movement-p mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil))))


;;;;; Font

(when (display-graphic-p)
  ;; Text rendering and scaling
  (setq x-underline-at-descent-line nil)
  (setq-default text-scale-remap-header-line t))

(use-package fontaine
  :demand t
  :wait t
  :if (display-graphic-p)

  :config

  (setopt fontaine-latest-state-file (expand-file-name "fontaine-latest-state.eld" ceamx-var-dir))

  ;; For some reason I do not yet understand, according to some hearsay, font
  ;; sizes best scale in multiples of 3-point increments. So, each height value
  ;; is a multiple of 3.
  (setopt fontaine-presets
    `((tiny
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

  ;; Persist latest preset across sessions.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))

(use-package ligature
  :demand t
  :wait t
  :if (display-graphic-p)

  :config

  ;; Enable all Iosevka ligatures in programming modes
  ;; <https://github.com/mickeynp/ligature.el/wiki#iosevka>
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                        "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                        "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                        ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))

  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;;;;; Focused View

;;;;;; =olivetti=: "Distraction-free" editing :package:

;; <https://github.com/rnkn/olivetti>

(use-package olivetti
  :ensure t
  :commands (olivetti-mode)
  :config
  (setopt olivetti-body-width 0.7
    olivetti-minimum-body-width 80
    olivetti-recall-visual-line-mode-entry-state t))

;;;;;; =logos=: a simple focus mode with page breaks or outlines :package:

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


;;;;; Miscellaneous

;; Differentiate between focused and non-focused windows
(setopt highlight-nonselected-windows nil)

;;;;; Keybindings

(define-keymap :keymap ceamx-session-prefix-map
  "a" (cons "Appearance" (define-prefix-command 'ceamx-session-appearance-prefix-command))
  "a f" #'fontaine-set-preset
  "a d" #'ceamx-ui/dark
  "a l" #'ceamx-ui/light
  "a o" #'olivetti-mode
  "a z" #'logos-focus-mode

  "f" (cons "Frame" (define-prefix-command 'ceamx-session-f-prefix))
  "f d" #'delete-frame)

;;;; Frame

(undelete-frame-mode 1)

(add-to-list 'default-frame-alist '(undecorated . t))

;;;; Keyboard

;;;;; ~free-keys~: Show free keybindings for modkeys or prefixes
;;
;; <https://github.com/Fuco1/free-keys>
;;
;; > If called with prefix argument C-u, you can specify a prefix map to be
;; > used, such as C-c or C-c C-x (these are specified as a string).

(use-package free-keys)

;;;;; Improve predictability of `keyboard-quit' behavior

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
    ((region-active-p)
      (keyboard-quit))
    ((derived-mode-p 'completion-list-mode)
      (delete-completion-window))
    ((> (minibuffer-depth) 0)
      (abort-recursive-edit))
    (t
      (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)


;;;;; `repeat-mode'

(use-feature! repeat
  :hook (ceamx-after-init . repeat-mode)

  :init
  (setopt repeat-exit-timeout 15
    repeat-on-final-keystroke t
    repeat-keep-prefix nil
    ;; Allow any key sequence to exit `repeat-mode'
    repeat-exit-key nil))

;; Related, but not technically part of `repeat-mode':
(setopt set-mark-command-repeat-pop t)



;;;; Buffer Management

(keymap-global-set "C-c b"
  (cons "Buffer" (define-prefix-command 'ceamx-buffer-prefix 'ceamx-buffer-prefix-map)))

;;;;; Variables

(defcustom ceamx-fallback-buffer-name "*scratch*"
  "The name of the buffer to fall back to if no other buffers exist.
The buffer will be created if it does not exist."
  :group 'ceamx
  :type '(string))

;;;;;; Define groups of commonly-related modes and buffer-name patterns

;; <https://github.com/karthink/.emacs.d/blob/6aa2e034ce641af60c317697de786bedc2f43a71/lisp/setup-windows.el>

(defvar ceamx-occur-grep-modes-list
  '(occur-mode
     grep-mode
     xref--xref-buffer-mode
     flymake-diagnostics-buffer-mode)
  "List of major-modes used in occur-type buffers.")

(defvar ceamx-repl-modes-list
  '(eshell-mode
    inferior-emacs-lisp-mode            ; ielm
    shell-mode
    eat-mode
    nix-repl-mode)
  "List of major-modes used in REPL buffers.")

(defvar ceamx-repl-buffer-names-list
  '("^\\*\\(?:.*?-\\)\\{0,1\\}e*shell[^z-a]*\\(?:\\*\\|<[[:digit:]]+>\\)$"
    "\\*.*REPL.*\\*"
    "\\*Inferior .*\\*$"
    "\\*ielm\\*"
    "\\*edebug\\*")
  "List of buffer names used in REPL buffers.")

(defvar ceamx-help-modes-list
  '(helpful-mode
    help-mode
    eldoc-mode)
  "List of major-modes used in documentation buffers.")

(defvar ceamx-help-buffer-names-list
  '("^\\*Apropos"
    "^\\*eldoc\\*")
  "List of buffer names used in help buffers.")

(defvar ceamx-manual-modes-list '(Man-mode woman-mode)
  "List of major-modes used in Man-type buffers.")

(defvar ceamx-message-modes-list
  '(compilation-mode
    edebug-eval-mode)
  "List of major-modes used in message buffers.")

(defvar ceamx-buffer-read-only-dirs-list
  (list ceamx-packages-dir)
  "List of directories whose files should be opened in read-only buffers.")

(defvar ceamx-checkers-buffer-names-regexp
  (rx "*" (or "Flycheck" "Package-Lint")))


;;;;; General buffer management customizations

(setq-default indicate-empty-lines nil)
(setq-default fill-column 80)

;; Available cycle positions for `recenter-top-bottom'.
(setopt recenter-positions '(top middle bottom))

;; Disable buffer line wrapping by default.
(setq-default truncate-lines t)

;;;;; Improve scrolling behavior

(setopt scroll-error-top-bottom t)

;; Prevent unwanted horizontal scrolling upon navigation.
(setopt scroll-preserve-screen-position t)

(setopt scroll-conservatively 10000)

;; Add a margin when scrolling vertically (or don't).
;; (setq-default scroll-margin 4)

(define-keymap :keymap (current-global-map)
  ;; The default bindings feel backwards to me.
  "C-x <" #'scroll-right
  "C-x >" #'scroll-left

  "<wheel-left>" #'scroll-left
  "<wheel-right>" #'scroll-right)

;;;;; Auto-revert buffers

;; Ensure the non-file-visiting buffers are also auto-reverted as needed.  For
;; example, this will cause Dired to refresh a file list when the directory
;; contents have changed.
(setopt global-auto-revert-non-file-buffers t)

(setopt auto-revert-interval 2)

;; Automatically revert a buffer if its file has changed on disk.
(add-hook 'ceamx-after-init-hook #'global-auto-revert-mode)

;;;;; `ibuffer'

(setopt ibuffer-movement-cycle t)

;;;;; Linkify web and email addresses

(use-feature! goto-addr
  :hook (prog-mode . goto-address-prog-mode))

;;;;; Disambiguate buffer names

(use-feature! uniquify
  :config
  (setopt uniquify-buffer-name-style 'forward)
  (setopt uniquify-separator "/")

  ;; Rename after killing uniquified buffer.
  (setopt uniquify-after-kill-buffer-p t)

  ;; Don't muck with special buffers.
  (setopt uniquify-ignore-buffers-re "^\\*"))


;;;;; `lentic' :: Create decoupled views of the same content

(use-package lentic
  :ensure t
  :config
  (add-to-list 'safe-local-variable-values '(lentic-init . lentic-orgel-org-init)))

;;;;; Keybindings for buffer management

(define-keymap :keymap (current-global-map)
  "C-c [" #'previous-buffer
  "C-c ]" #'next-buffer
  "C-c `" #'mode-line-other-buffer

  "C-x k" #'ceamx/kill-this-buffer      ; orig: `kill-buffer'
  "C-x K" #'kill-buffer
  "C-x C-b" #'ibuffer)

(define-keymap :keymap ceamx-buffer-prefix-map
  "b" #'consult-buffer
  "k" #'ceamx/kill-this-buffer)


;;;; Window Management

(require 'ceamx-window-lib)

(keymap-global-set "C-c w"
  (cons "Window" (define-prefix-command 'ceamx-window-prefix 'ceamx-window-prefix-map)))

;;;;; General buffer display settings

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

;;;;; Declare rules for displaying buffers with `display-buffer-alist'

;; <https://github.com/karthink/.emacs.d/blob/6aa2e034ce641af60c317697de786bedc2f43a71/lisp/setup-windows.el>

;; <karthink> has a helpful summary of `display-buffer' action functions and
;; alist entries in their Emacs configuration, which I am also including here
;; for my own reference.  Note that this list is not necessarily complete.

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

          (,ceamx-checkers-buffer-names-regexp
           (display-buffer-in-direction
            display-buffer-in-side-window)
           (window-parameters . ((no-other-window . t))))

          ;; TODO: is there not a simpler way than using `ceamx-buffer-mode'?
          ;; e.g. `derived-mode-p' or similar
          ((lambda (buf act) (member (ceamx-buffer-mode buf) ceamx-message-modes-list))
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

;;;;; Handle popup windows

;; <https://github.com/karthink/popper>

(use-package popper
  :init
  (define-keymap :keymap (current-global-map)
    "C-`" #'popper-toggle
    "C-~" #'popper-cycle
    "C-M-`" #'popper-toggle-type)

  (setopt popper-reference-buffers
    (append
      ceamx-help-modes-list
      ceamx-help-buffer-names-list
      ceamx-manual-modes-list
      ceamx-repl-modes-list
      ceamx-repl-buffer-names-list
      ceamx-occur-grep-modes-list
      '(+popper-current-buffer-popup-p)
      '(Custom-mode
         compilation-mode
         messages-buffer-mode)
      (list
        ceamx-checkers-buffer-names-regexp)

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
  (popper-echo-mode)

  :config
  (defvar-keymap popper-repeat-map
    :repeat t
    "`" #'popper-cycle
    "~" #'popper-cycle-backwards))

;;;;; Allow restoring previous window configurations

(use-feature! winner
  :hook (ceamx-after-init . winner-mode))

;;;;; Add toggle for a window's "dedicated" flag

(keymap-set ceamx-toggle-map "W" #'toggle-window-dedicated)

;;;;; `ace-window' :: Interactively manage windows

;; <https://github.com/abo-abo/ace-window>

(use-package ace-window
  :ensure t

  :config
  ;; Same frame only.  While it'd be nice to use the default (global), I really
  ;; dislike that it orders window numbers leads to jarring gaps in window
  ;; numbers in the same frame.  For example, frame A might have windows
  ;; numbered 1 and 3 and frame B will have window 2.
  (setopt aw-scope 'frame))


;;;;; `transpose-frame' :: Rotate a frame's composite windows

(use-package transpose-frame
  :ensure t
  :init
  (keymap-global-set "C-c w SPC" #'transpose-frame))

;;;;; Define the `ceamx/window-dispatch' window management menu

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

;;;;; Keybindings for window management

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

  "=" #'balance-windows
  "SPC" #'ceamx/swap-or-rotate-windows)

(define-keymap :keymap (current-global-map)
  "C-x o" #'ceamx/other-window
  "C-x O" #'ace-window

  "C-x =" #'balance-windows
  "C-x +" #'balance-windows-area

  "C-x C-n" #'next-buffer
  "C-x C-p" #'previous-buffer

  "C-x <up>" #'enlarge-window           ; also: C-x ^
  "C-x <down>" #'shrink-window
  "C-x <left>" #'shrink-window-horizontally
  "C-x <right>" #'enlarge-window-horizontally)

;;;;; Bind repeatable keys for resizing windows

(define-keymap :keymap resize-window-repeat-map
  "<up>" #'enlarge-window
  "<down>" #'shrink-window
  "<left>" #'shrink-window-horizontally
  "<right>" #'enlarge-window-horizontally)

;;;;; Bind repeatable keys for window actions

;; This is very similar to `window-prefix-map'.  Unfortunately, it does not seem
;; possible to create a functional `repeat-map' inheriting from a parent keymap.
;; The commands bound in the parent map are unaffected by the `defvar-keymap'
;; `:repeat' keyword of a child map.

(defvar-keymap ceamx-window-repeat-map
  :repeat t

  "0" #'delete-window
  "2" #'split-window-below
  "3" #'split-window-right

  "b" #'consult-buffer
  "f" #'find-file

  "o" #'ceamx/other-window
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

  "SPC" #'ceamx/swap-or-rotate-windows

  "RET" #'repeat-exit
  "ESC" #'repeat-exit)


;;;; Navigation

(use-package avy
  :init
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

(use-package link-hint
  :ensure t
  :init
  (define-keymap :keymap (current-global-map)
    "M-g u" #'link-hint-open-link
    "M-g U" #'link-hint-copy-link))

(use-package mwim
  :ensure t
  :demand t
  :init
  (keymap-global-set "C-a" #'mwim-beginning)
  (keymap-global-set "C-e" #'mwim-end))

(use-package expand-region
  :ensure t
  :init
  (keymap-global-set "C-=" #'er/expand-region))


;;;; Chaos



;;; The End


;; Local Variables:
;; indent-tabs-mode: nil
;; no-native-compile: t
;; sentence-end-double-space: t
;; End:
