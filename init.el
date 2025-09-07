;;; init.el --- Ceamx -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords:

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

;;

;;; Code:


;;;; Requirements

(require 'cl-lib)

(require 'ceamx-paths)
(require 'ceamx-lib)

(setq load-path (append (ceamx-subdirs ceamx-lisp-dir)
                        (ceamx-subdirs ceamx-site-lisp-dir)
                        load-path))

(require 'ceamx-lisp)


;;;; Bootstrap

(setq custom-file (locate-user-emacs-file "custom.el"))

(setq-default user-full-name "Chris Montgomery"
              user-mail-address "chmont@protonmail.com")


;;;; Customization

(defgroup ceamx nil
  "User settings for Ceamx."
  :group 'emacs)

(defcustom ceamx-agenda-dir
  (file-name-as-directory (concat ceamx-notes-dir "g2d"))
  "Base directory for Org-Agenda."
  :type 'directory
  :group 'ceamx)

(defcustom ceamx-default-agenda-files
  (file-expand-wildcards (file-name-concat ceamx-agenda-dir "*.org"))
  "List of absolute paths of all files to include in the agenda."
  :type '(repeat file)
  :group 'ceamx)

(defcustom ceamx-default-todo-file
  (expand-file-name "todo.org" ceamx-agenda-dir)
  "Absolute path to default Something-Doing file."
  :type 'file
  :group 'ceamx)

(defcustom ceamx-outline-search-max-level 5
  "Maximum level to search in outlines."
  :type 'number
  :group 'ceamx)

(defcustom ceamx-repl-key "C-:"
  "Key sequence for mode-specific REPL commands."
  :type '(key)
  :group 'ceamx)

(defcustom ceamx-buffer-read-only-dirs-list (list ceamx-packages-dir)
  "List of directories whose files should be opened in read-only buffers."
  :group 'ceamx
  :type '(string))

(defcustom ceamx-text-mode-derived-prog-modes
  '(nxml-mode sgml-mode toml-ts-mode yaml-mode)
  "Programming modes who are sadly derived from `text-mode'."
  :type '(repeat symbol)
  :group 'ceamx)

(defcustom ceamx-typo-mode-excluded-modes nil
  "Modes where `typo-mode' should not be enabled."
  :type '(symbol)
  :group 'ceamx)

(defcustom ceamx-checkers-buffer-names-regexp
  (rx "*" (or "Flycheck" "Package-Lint"))
  "Regular expression matching buffer names for checker buffers."
  :type 'regexp
  :group 'ceamx)

(defcustom ceamx-grep-modes-list
  '(occur-mode
    grep-mode
    xref--xref-buffer-mode
    flymake-diagnostics-buffer-mode)
  "List of major-modes used in occur-type buffers."
  :type '(repeat symbol)
  :group 'ceamx)

(defcustom ceamx-repl-modes-list
  '(eshell-mode
    inferior-emacs-lisp-mode            ; ielm
    shell-mode
    eat-mode
    nix-repl-mode)
  "List of major-modes used in REPL buffers."
  :type '(repeat symbol)
  :group 'ceamx)

(defcustom ceamx-repl-buffer-names-list
  '("^\\*\\(?:.*?-\\)\\{0,1\\}e*shell[^z-a]*\\(?:\\*\\|<[[:digit:]]+>\\)$"
    "\\*.*REPL.*\\*"
    "\\*Inferior .*\\*$"
    "\\*ielm\\*"
    "\\*edebug\\*")
  "List of buffer names used in REPL buffers."
  :type '(repeat string)
  :group 'ceamx)

(defcustom ceamx-help-modes-list
  '(helpful-mode
    help-mode
    eldoc-mode)
  "List of major-modes used in documentation buffers."
  :type '(repeat symbol)
  :group 'ceamx)

(defcustom ceamx-help-buffer-names-list
  '("^\\*Apropos"
    "^\\*eldoc\\*")
  "List of buffer names used in help buffers."
  :type '(repeat string)
  :group 'ceamx)

(defcustom ceamx-manual-modes-list '(Man-mode woman-mode)
  "List of major-modes used in Man-type buffers."
  :type '(repeat symbol)
  :group 'ceamx)

(defcustom ceamx-message-modes-list
  '(compilation-mode
    edebug-eval-mode)
  "List of major-modes used in message buffers."
  :type '(repeat symbol)
  :group 'ceamx)


;;;; Startup

(menu-bar-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)


;;;; Security

(setq ffap-machine-p-known 'reject)
(setq gnutls-verify-error t)
(setq gnutls-min-prime-bits 3072)


;;;; Keymaps

(define-prefix-command 'ceamx-activities-prefix)
(define-prefix-command 'ceamx-appearance-prefix)
(define-prefix-command 'ceamx-buffer-prefix)
(define-prefix-command 'ceamx-bookmark-prefix)
(define-prefix-command 'ceamx-capture-prefix)
(define-prefix-command 'ceamx-code-prefix)
(define-prefix-command 'ceamx-completion-prefix)
(define-prefix-command 'ceamx-cryption-prefix)
(define-prefix-command 'ceamx-export-prefix)
(define-prefix-command 'ceamx-file-prefix)
(define-prefix-command 'ceamx-fold-prefix)
(define-prefix-command 'ceamx-help-keybindings-prefix)
(define-prefix-command 'ceamx-history-prefix)
(define-prefix-command 'ceamx-info-prefix)
(define-prefix-command 'ceamx-insert-prefix)
(define-prefix-command 'ceamx-launch-prefix)
(define-prefix-command 'ceamx-note-prefix)
(define-prefix-command 'ceamx-journal-prefix)
(define-prefix-command 'ceamx-package-prefix)
(define-prefix-command 'ceamx-replace-prefix)
(define-prefix-command 'ceamx-session-prefix)
(define-prefix-command 'ceamx-snippet-prefix)
(define-prefix-command 'ceamx-structural-editing-prefix)
(define-prefix-command 'ceamx-toggle-prefix)
(define-prefix-command 'ceamx-web-prefix)
(define-prefix-command 'ceamx-workspace-prefix)


;;;; Packages Setup

(require 'package)

(setq package-user-dir ceamx-packages-dir)
(setq package-native-compile t)
;; Too flaky.  As long as package archives use HTTPS, no worries.
(setq package-check-signature nil)

(push '("melpa" . "https://melpa.org/packages/") package-archives)
;; Official MELPA Mirror, in case necessary.
;; (push '("melpa-mirror" . "https://www.mirrorservice.org/sites/melpa.org/packages/")
;;       package-archives)

(package-initialize)

(let ((package-check-signature nil))
  (unless (package-installed-p 'gnu-elpa-keyring-update)
    (package-install 'gnu-elpa-keyring-update)))

(progn
  ;; These must be set before the package loads.
  (eval-and-compile
    (setq no-littering-etc-directory ceamx-storage-dir
          no-littering-var-directory ceamx-cache-dir))
  (unless (package-installed-p 'no-littering)
    (package-install 'no-littering))
  (require 'no-littering)
  (setq history-file (expand-file-name "history" ceamx-cache-dir))
  (setq recentf-save-file (expand-file-name "recentf" ceamx-cache-dir))
  (setq bookmark-default-file (expand-file-name "bookmarks" ceamx-cache-dir))
  (setq project-list-file (expand-file-name "projects" ceamx-cache-dir))
  (with-eval-after-load 'recentf
    (cl-pushnew (recentf-expand-file-name no-littering-var-directory) recentf-exclude)
    (cl-pushnew (recentf-expand-file-name no-littering-etc-directory) recentf-exclude)))


;;;;; setup.el

;; <https://www.emacswiki.org/emacs/SetupEl>
(progn
  (unless (package-installed-p 'setup)
    (package-vc-install '(setup . (:url "https://codeberg.org/pkal/setup.el"))))
  (require 'setup)

  (setup-define :load-after
    (lambda (&rest features)
      (let ((body `(require ',(setup-get 'feature))))
        (dolist (feature (nreverse features))
          (setq body `(with-eval-after-load ',feature ,body)))
        body))
    :documentation "Load the current feature after FEATURES."))

;; Override the builtin ":hook" macro to accept priority.
;; FIXME: "ensure" spec causes error...
;; (setup-define :hook
;;   (lambda (function &optional depth)
;;     `(add-hook ',(setup-get 'hook) ,function ,depth))
;;   :documentation "Add FUNCTION to current hook with optional DEPTH."
;;   :ensure '(func nil)
;;   :repeatable t)

;;;;; Install the latest version of Org-Mode

(unless after-init-time
  (when (and (featurep 'org)
             (package--active-built-in-p 'org))
    (package-upgrade 'org)))


;;;; Libraries

(setup transient
  (:when-loaded
    (:with-map transient-map
      (:bind "<escape>" #'transient-quit-one))))

(setup (:package llama)
  (require 'llama))

(setup (:package f))

(setup (:package tmr)
  (:with-feature embark
    (:when-loaded
      (defvar-keymap ceamx+embark+tmr-action-map
	:doc "Action map for TMR timers"
	"k" #'tmr-remove
	"r" #'tmr-remove
	"R" #'tmr-remove-finished
	"c" #'tmr-clone
	"a" #'tmr-toggle-acknowledge
	"e" #'tmr-edit-description
	"s" #'tmr-reschedule)
      (cl-pushnew '(tmr-timer . ceamx+embark+tmr-action-map) embark-keymap-alist)
      (cl-loop
       for cmd the key-bindings of ceamx+embark+tmr-action-map
       if (commandp cmd) do
       (cl-pushnew (list cmd 'embark--restart) embark-post-action-hooks)))))

(setup (:package uuidgen))


;;;; Environment

(setup (:package exec-path-from-shell)
  (require 'exec-path-from-shell)
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var)))

(setup (:package inheritenv)
  (:with-feature exec-path-from-shell
    (:when-loaded
      (require 'inheritenv))))

(setup (:package envrc)
  (:with-feature exec-path-from-shell
    (:when-loaded
      (envrc-global-mode))))


;;;; Input Methods

(setup emacs
  (set-language-environment "UTF-8")
  (setq! default-input-method nil))

(setup mouse
  ;; (:with-feature ceamx-simple
  ;;   (:with-function '(ceamx/scroll-down ceamx/scroll-up)
  ;;     (:autoload-this))
  ;;   (keymap-global-set "<mouse-4>" #'ceamx/scroll-down)
  ;;   (keymap-global-set "<mouse-5>" #'ceamx/scroll-up))
  (setq! mouse-drag-and-drop-region-cross-program t)
  (mouse-avoidance-mode 'exile)
  (unless (display-graphic-p)
    (xterm-mouse-mode 1)))

;;;;; Repeat Mode

(setup repeat
  (setq! repeat-exit-timeout 15
         repeat-exit-key "<return>")
  (setq! repeat-keep-prefix t)
  (repeat-mode 1)
  (:with-feature emacs
    (setq! set-mark-command-repeat-pop t)))


;;;; Appearance

(require 'ceamx-ui)

;;;;; Theme

(setup emacs
  ;; Consider all themes "safe".
  (setq! custom-safe-themes t)
  (setq! custom-theme-allow-multiple-selections nil))

;;;;;; Standard Themes

(setup (:package standard-themes)
  (require 'standard-themes)
  (:with-feature ceamx-ui
    (:when-loaded
      (ceamx-ui-define-preferred-themes 'standard 'standard-dark 'standard-light)
      (setq! standard-themes-to-toggle
             (ceamx-ui-theme-family-preferred-themes 'standard))))
  (setq! standard-themes-bold-constructs t
         standard-themes-italic-constructs t
         standard-themes-mixed-fonts t
         standard-themes-variable-pitch-ui t)
  (setq! standard-themes-prompts '(extrabold italic)))

;;;;;; Doric Themes

(setup (:package doric-themes)
  (require 'doric-themes)
  (:with-feature ceamx-ui
    (:when-loaded
      (ceamx-ui-define-preferred-themes 'doric 'doric-dark 'doric-light)
      (setq! doric-themes-to-toggle
             (ceamx-ui-theme-family-preferred-themes 'doric)))))

;;;;;; Modus Themes

(setup (:package modus-themes)
  (require 'modus-themes)
  (ceamx-ui-define-preferred-themes 'modus 'modus-vivendi 'modus-operandi)
  (setq! modus-themes-to-toggle (ceamx-ui-theme-family-preferred-themes 'modus))
  (setq! modus-themes-bold-constructs t
         modus-themes-italic-constructs t
         modus-themes-mixed-fonts t
         modus-themes-variable-pitch-ui t)
  (setq! modus-themes-prompts '(italic bold))
  (setq! modus-themes-completions
         '((matches . (extrabold underline))
           (selection . (semibold italic)))))

;;;;;; Ef Themes

(setup (:package ef-themes)
  (require 'ef-themes)
  (ceamx-ui-define-preferred-themes 'ef 'ef-winter 'ef-frost)
  (setq! ef-themes-to-toggle (ceamx-ui-theme-family-preferred-themes 'ef))
  (setq! ef-themes-mixed-fonts t
         ef-themes-variable-pitch-ui t))

;;;;;; Load the preferred theme

(setup ceamx-ui
  (setq! ceamx-ui-theme-family 'modus))

;;;;; Font

(setup emacs
  (setq-default text-scale-remap-header-line t))

(setup (:package fontaine)
  (:only-if (display-graphic-p))
  (require 'fontaine)
  (setq! fontaine-latest-state-file
         (expand-file-name "fontaine-latest-state.eld" ceamx-storage-dir))
  (setq! fontaine-presets
         `((tiny
	    :default-height 78)
           (small
	    :default-height 90)
           (regular
	    :default-height 102)
           (medium
	    :default-height 117)
           (large
	    :default-height 133)
           (huge
	    :default-height 155)
           (t
	    ;; Inherit the default font from the window manager.
	    :default-family "Monospace"
	    :default-height 94
            :fixed-pitch-family "Monospace"
            :variable-pitch-family "Aporetic Serif"
            ;; :variable-pitch-family "Charis SIL"
            ;; :variable-pitch-family "iA Writer Duospace"
            :variable-pitch-height 1.1
            :mode-line-active-family "Berkeley Mono"
            :mode-line-active-height 0.8
            :mode-line-inactive-family "Berkeley Mono"
            :mode-line-inactive-height 0.8
            :header-line-family "Berkeley Mono"
            :header-line-height 0.8
            :line-number-family "Berkeley Mono"
            :line-number-height 0.8
            :tab-bar-family "Berkeley Mono"
            :tab-bar-height 0.8
            :tab-line-family "Berkeley Mono"
            :tab-line-height 0.8
	    :line-spacing 0.01)))
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (fontaine-mode 1))

(setup (:package ligature)
  (:only-if (display-graphic-p))
  (:with-feature fontaine
    (:when-loaded
      (global-ligature-mode 1)))
  (:when-loaded
    (ligature-set-ligatures
     'prog-mode
     '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->"
       "<---->" "<!--" "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">="
       "<=>" "<==>" "<===>" "<====>" "<!---" "<~~" "<~" "~>" "~~>"
       "::" ":::" "==" "!=" "===" "!==" ":=" ":-" ":+" "<*" "<*>"
       "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))))

(setup (:package show-font)
  (:only-if (display-graphic-p))
  (setq! show-font-pangram 'prot))


;;;;; Layout

(setup (:package spacious-padding)
  (:hook-into after-init-hook)
  (setq! spacious-padding-widths
         '( :internal-border-width 4
            :header-line-width 2
            :mode-line-width 2
            :tab-width 4
            :right-divider-width 6
            :scroll-bar-width 6
            :left-fringe-width 2
            :right-fringe-width 2))
  (setq! spacious-padding-subtle-frame-lines nil))


;;;;; Decorations

(setup (:package nerd-icons)
  (require 'nerd-icons)
  (setq! nerd-icons-font-family "Symbols Nerd Font Mono"))

(setup (:package page-break-lines)
  (:with-hook after-init-hook
    (:hook #'global-page-break-lines-mode)))

(setup (:package svg-lib)
  (:when-loaded
    (plist-put svg-lib-style-default :padding 2)
    (plist-put svg-lib-style-default :margin 2)
    (plist-put svg-lib-style-default :height 0.9)))


;;;;; Feedback

(setup hl-line
  (:with-mode prog-mode
    (:hook #'hl-line-mode))
  ;; Disable line highlight in unfocused windows.
  (setq! hl-line-sticky-flag nil))

(setup (:package lin)
  (:with-hook after-init-hook
    (:hook #'lin-global-mode)))

(setup (:package pulsar)
  (:with-hook after-init-hook
    (:hook #'pulsar-global-mode))
  (:with-hook minibuffer-setup-hook
    (:hook #'pulsar-pulse-line))

  (setq! pulsar-pulse t
	 pulsar-delay 0.055
	 pulsar-iterations 30)
  (setq! pulsar-face 'pulsar-generic
	 pulsar-highlight-face 'pulsar-face)

  (with-eval-after-load 'pulsar
    (dolist (fn '( recenter-top-bottom move-to-window-line-top-bottom reposition-window
                   bookmark-jump other-window delete-window delete-other-windows
                   forward-page backward-page scroll-up-command scroll-down-command
                   tab-new tab-close tab-next outline-backward-same-level
                   outline-forward-same-level outline-next-heading outline-next-visible-heading
                   outline-previous-heading outline-previous-visible-heading
                   outline-up-heading))
      (cl-pushnew fn pulsar-pulse-functions))

    (dolist (fn '(pulsar-pulse-line-red
                  pulsar-recenter-top
                  pulsar-reveal-entry))
      (add-hook 'next-error-hook #'fn))))

(setup (:package cursory)
  (require 'cursory)
  (add-hook 'after-init-hook
	    (defun ceamx-enable-cursory-mode ()
	      (cursory-mode 1)
	      (cursory-set-preset (or (cursory-restore-latest-preset) 'box))))
  (setq! cursory-latest-state-file (expand-file-name "cursory-latest-state.eld" ceamx-storage-dir))
  (setq! cursory-presets
	 '((box
	    :blink-cursor-interval 0.2)
	   (bar
	    :cursor-type (bar . 2)
	    :blink-cursor-interval 0.6)
	   (t
	    :cursor-type box
	    :cursor-in-non-selected-windows hollow
	    :blink-cursor-mode 1
	    :blink-cursor-blinks 33
	    :blink-cursor-interval 0.4
	    :blink-cursor-delay 0.2))))

;;;;; Images

(setup image-mode
  (setq! image-animate-loop t))

;;;;; Focus

(setup (:package olivetti)
  (add-hook 'olivetti-mode-on-hook
	    (defun +olivetti-mode-on--disable-conflicting-features-h ()
	      (when (fboundp 'diff-hl-mode)
		(diff-hl-mode -1)))))


;;;; Frame

(setup emacs
  (setq! fit-frame-to-buffer t)
  (undelete-frame-mode 1))


;;;; Window

(require 'ceamx-window)

(setup window
  (setq! split-width-threshold 120
         split-height-threshold nil)
  (setq! help-window-select t
         Man-notify-method 'aggressive))

(setup winner
  (:hook-into after-init-hook))

;;;;; Scrolling

(setup window
  (setq! recenter-positions '(top middle bottom))
  (setq! auto-hscroll-mode 'current-line)
  (setq! ;; scroll-error-top-bottom t
   ;; scroll-preserve-screen-position t
   scroll-conservatively 10000))

;;;;; Display buffer

(setup window
  ;; Hide this buffer until there is output to show.
  (setq! async-shell-command-display-buffer nil)
  (setq! switch-to-buffer-in-dedicated-window 'pop)
  (setq! switch-to-buffer-obey-display-actions t)
  (setq! window-resize-pixelwise t)

  (setq! display-buffer-base-action
         '((display-buffer-reuse-window display-buffer-in-previous-window)))

  (setq! display-buffer-alist
         `( (,ceamx-checkers-buffer-names-regexp
             ( display-buffer-in-direction
               display-buffer-in-side-window)
             (window-parameters . ((no-other-window . t))))

            ((lambda (buf act)
               ;; TODO: double-check this
               (member (derived-mode-p (with-current-buffer buf major-mode))
                       ceamx-message-modes-list))
             ( display-buffer-at-bottom
               display-buffer-in-side-window))

            (,(rx "*" (group (or "Compile-Log" "Messages" "Warnings")) "*")
             ( display-buffer-at-bottom
               display-buffer-in-side-window
               display-buffer-in-direction))

            (,(rx "*" (group (or "Backtrace")) "*")
             ( display-buffer-in-side-window)
             (window-height . 0.2)
             (side . bottom)))))

;;;;; Popups

(setup (:package popper)
  (setq! popper-reference-buffers
         (append
          ceamx-help-modes-list
          ceamx-help-buffer-names-list
          ceamx-manual-modes-list
          ceamx-repl-modes-list
          ceamx-repl-buffer-names-list
          ceamx-grep-modes-list
          '( compilation-mode
             epa-info-mode
             messages-buffer-mode)
          (list ceamx-checkers-buffer-names-regexp)
          `(,(rx "Output*" eol)
            ,(rx "*" (or
                      "Async-native-compile-log"
                      "Backtrace"
                      "Compile-Log"
                      "Completions"
                      "compilation"
                      "elpaca-diff"
                      "Error"
                      "Messages"
                      "Shell Command Output"
                      "vc"
                      "Warnings")
                 "*")
            "^\\*Embark Export"
            "^Calc:"
            "\\*Async Shell Command\\*"
            ;; ("\\*Async Shell Command\\*" . hide)
            ("\\*Detached Shell Command\\*" . hide))))
  (popper-mode)
  (popper-echo-mode)
  (defvar-keymap popper-repeat-map
    :repeat t
    "`" #'popper-cycle
    "~" #'popper-cycle-backwards))

;;;;; Window management

(setup (:package golden-ratio)
  (:hook-into after-init-hook)
  (setq! golden-ratio-auto-scale t))

(setup (:package ace-window)
  (setq! aw-scope 'visible)
  (:with-feature pulsar
    (:when-loaded
      (dolist (fn '( aw-copy-window aw-delete-window aw-move-window
                     aw-split-window-fair aw-split-window-horz
                     aw-split-window-vert aw-swap-window))
        (cl-pushnew fn pulsar-pulse-functions)))))

(setup (:package transpose-frame))


;;;; Buffer

(setup emacs
  (:with-hook ( prog-mode-hook text-mode-hook)
    (:hook #'auto-fill-mode))
  (:with-hook prog-mode-hook
    (:hook (defun ceamx-prog-mode-auto-fill-comments-only-h ()
             (setq-local comment-auto-fill-only-comments t))))
  (setq-default
   fill-column 72
   truncate-lines t)
  (setq!
   uniquify-buffer-name-style 'post-forward-angle-brackets
   uniquify-separator "/"
   uniquify-ignore-buffers-re "^\\*"))

(setup autorevert
  (setq! global-auto-revert-non-file-buffers t)
  (setq! auto-revert-interval 2)
  (global-auto-revert-mode 1))

(setup goto-addr
  (:with-hook prog-mode-hook
    (:hook #'goto-address-prog-mode)))

(setup (:package link-hint))

;;;;; `ibuffer'


;;;; Tabs

(setup tab-bar
  (tab-bar-mode 1)
  (setq! tab-bar-auto-width-max '((120) 20)))


;;;; Workspaces

(setup (:package activities)
  (activities-mode 1)
  (activities-tabs-mode 1)
  (setq! activities-bookmark-store nil)
  (setq! activites-kill-buffers t))


;;;; Mode Line

(setup emacs
  (column-number-mode 1))

(setup (:package mlscroll)
  (:hook-into after-init-hook))

(setup (:package minions)
  (:hook-into after-init-hook))


;;;; Header Line

(setup (:package breadcrumb)
  (:hook-into after-init-hook))


;;;; Help

(setup info
  (:with-hook info-mode-hook
    (:hook #'hl-line-mode)
    (:hook #'scroll-lock-mode)))

(setup eldoc
  (setq! eldoc-documentation-function #'eldoc-documentation-compose)
  (advice-add #'elisp-get-var-docstring :around
              (defun ceamx+eldoc-append-value-a (fn sym)
                "Display variable value next to docstring in eldoc."
                (when-let (ret (funcall fn sym))
                  (if (boundp sym)
                      (concat ret " "
                              (let* ( (truncated " [...]")
                                      (print-escape-newlines t)
                                      (str (symbol-value sym))
                                      (str (prin1-to-string str))
                                      (limit (- (frame-width) (length ret) (length truncated) 1)))
                                (format (format "%%0.%ds%%s" (max limit 0))
                                        (propertize str 'face 'warning)
                                        (if (< (length str) limit) "" truncated))))
                    ret)))))

(setup keyboard
  (:with-feature embark
    (:when-loaded
      (setq! prefix-help-command #'embark-prefix-help-command)
      (:with-feature vertico
	(:when-loaded
	  (cl-pushnew '(embark-keybinding grid) vertico-multiform-categories))))))

(setup (:package helpful)
  (require 'helpful))

(setup (:package elisp-demos)
  (:with-feature helpful
    (:when-loaded
      (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update))))

(setup (:package devdocs)
  (:with-hook after-init-hook
    (:hook #'devdocs-update-all))
  (:with-feature popper
    (:when-loaded
      (cl-pushnew "\\*devdocs\\*" popper-reference-buffers))))


;;;; Secrets

;;;;; Auth Source

(require 'ceamx-auth)

(setup auth-source
  (require 'auth-source)
  ;; Disallow unencrypted authinfo file.
  (setq! auth-sources (list "~/.authinfo.gpg")))

(setup auth-source-pass
  (require 'auth-source-pass)
  (auth-source-pass-enable))

;;;;; GnuPG

(setup epa
  (require 'epa)
  ;; Enable automatic encryption on GPG files.
  (epa-file-enable))

(setup epg
  (setq! epg-pinentry-mode 'loopback))

;;;; Essentials

(setup emacs
  (setq! message-log-max 10000)
  (setq! auto-mode-case-fold nil))


;;;; History

(setup savehist
  (require 'savehist)
  (dolist (var '(kill-ring
                 regexp-search-ring
                 search-ring
                 register-alist))
    (cl-pushnew var savehist-additional-variables))
  (setq! history-length 333
         history-delete-duplicates nil)
  (setq! savehist-autosave-interval 30)
  (savehist-mode 1))

(setup saveplace
  (save-place-mode 1))

(setup recentf
  (require 'recentf)
  (setq! recentf-max-menu-items 20
         recentf-max-saved-items 50)
  (setq! recentf-auto-cleanup 'never)
  (dolist (path '(ceamx-storage-dir ceamx-cache-dir))
    (cl-pushnew path recentf-exclude))
  (recentf-mode 1))

(setup undo
  (setq! undo-limit (* 64 (* 1024 1024))
         undo-strong-limit (* 96 (* 1024 1024))
         undo-outer-limit (* undo-strong-limit 100)))

(setup (:package undo-fu-session)
  (require 'undo-fu-session)
  (setq! undo-fu-session-directory (expand-file-name "undo-fu-session" ceamx-storage-dir))
  (setq! undo-fu-session-incompatible-files '("/git-rebase-todo\\'"))
  (setq! undo-fu-session-ignore-temp-files t
         undo-fu-session-ignore-encrypted-files t)
  (setq! undo-fu-session-compression 'zst)
  (undo-fu-session-global-mode))

(setup (:package vundo)
  (:when-loaded
    (setq! vundo-glyph-alist vundo-unicode-symbols)))


;;;; Bookmarks

(setup (:package bookmark-in-project))


;;;; Editing

(setup emacs
  (setq! save-interprogram-paste-before-kill t)
  ;; Replace region when inserting text.
  (delete-selection-mode 1)
  (global-subword-mode 1))

(setup (:package crux)
  (:with-feature pulsar
    (:when-loaded
      (cl-pushnew #'crux-other-window-or-switch-buffer pulsar-pulse-functions))))

(setup (:package easy-kill))

(setup (:package mwim))

(setup (:package drag-stuff))

(setup (:package editorconfig)
  (:with-hook after-init-hook
    (:hook #'editorconfig-mode))
  (:with-hook editorconfig-after-apply-functions
    (:hook
     ;; via <https://github.com/doomemacs/doomemacs/commit/43870bf8318f6471c4ce5e14565c9f0a3fb6e368>
     (defun +editorconfig-enforce-org-mode-tab-width-h (props)
       "Prevent `editorconfig' from changing `tab-width' in `org-mode'.
A \"tab-width\" of any value other than 8 is an error state in
org-mode, so it must not be changed.

PROPS is as in `editorconfig-after-apply-functions'."
       (when (and (gethash 'indent_size props)
                  (derived-mode-p 'org-mode))
         (setq tab-width 8)))))
  ;; This is super important! Yikes...
  (setq! editorconfig-lisp-use-default-indent t))

(setup (:package ialign))

(setup (:package string-inflection))

(setup (:package shift-number))

(setup (:package cycle-quotes))

(setup (:package smart-newline)
  (:hook-into prog-mode-hook))

;;;;; Character pair handling

(setup emacs
  (setq! blink-matching-paren 'jump)
  (setq! show-paren-style 'mixed)
  (show-paren-mode 1))

(setup electric
  (setq! electric-pair-open-newline-between-pairs t)
  (electric-pair-mode 1)
  ;; `electric-quote-mode' actually misbehaves quite badly, inserting
  ;; "smart quotes" where it should not (kind of like `typo-mode' but
  ;; worse, because `describe-key' ends up reporting that such-and-such
  ;; quote key is bound to `self-insert-command' despite appearances to
  ;; the contrary!).
  (electric-quote-mode -1))

;;;;; Whitespace and indentation handling

(setup emacs
  (:with-hook before-save-hook
    (:hook #'delete-trailing-whitespace))
  (:with-hook prog-mode-hook
    (:hook #'whitespace-mode))
  (setq-default indent-tabs-mode nil
                tab-width 8
                )
  (setq! backward-delete-char-untabify-method 'hungry)
  (setq! cycle-spacing-actions '(delete-all-space just-one-space restore))
  (setq! sentence-end-double-space t)
  (setq! kill-whole-line t)
  (setq! whitespace-line-column 100)
  (setq! whitespace-style
         '( face tabs spaces trailing-whitespace space-before-tab lines-tail
            empty space-after-tab space-before-tab missing-newline-at-eof
            )))

(setup electric
  (electric-indent-mode 1)
  (electric-layout-mode 1))

(setup (:package aggressive-indent)
  (global-aggressive-indent-mode 1))

;;;; Search

(setup emacs
  (setq-default case-fold-search t)
  (setq! find-library-include-other-files nil))

(setup xref
  (setq! xref-prompt-for-identifier nil))

(setup (:package avy)
  (setq! avy-style 'at-full)
  (setq! avy-all-windows t)
  (setq! avy-case-fold-search t)
  ;; Prevent conflict with themes.
  (setq! avy-background nil)
  (setq! avy-timeout-seconds 0.30))

(setup (:package substitute)
  (:with-hook substitute-post-replace-functions
    (:hook #'substitute-report-operation)))

(setup (:package wgrep)
  (:with-function wgrep-change-to-wgrep-mode
    (:autoload-this)))

;;;;; `isearch'

(setup isearch
  (setq! isearch-lazy-count t
         lazy-count-prefix-format "[%s/%s] ")
  (setq! isearch-repeat-on-direction-change t)
  (:bind "M-<" #'isearch-beginning-of-buffer
         "M->" #'isearch-end-of-buffer
         "M-/" #'isearch-complete
         "M-e" nil
         "M-w" #'isearch-yank-word-or-char
         "M-s <" #'isearch-beginning-of-buffer
         "M-s >" #'isearch-end-of-buffer
         "C-g" #'isearch-cancel
         "C-w" nil)
  (:with-map minibuffer-local-isearch-map
    (:bind "M-/" #'isearch-complete-edit)))


;;;; Version Control

;;;;; `vc-mode'

(setup vc
  (require 'vc)
  (setq! vc-follow-symlinks t))

(setup vc-annotate
  (setq! vc-annotate-display-mode 'scale)
  (:bind
   "RET" #'vc-annotate-find-revision-at-line
   "C-c C-c" #'vc-annotate-goto-line
   "M-q" #'vc-annotate-toggle-annotation-visibility))

(setup log-edit
  ;; FIXME: sometimes it does not get loaded automatically
  (require 'log-edit)
  (setq! log-edit-confirm 'changed)
  (setq! log-edit-setup-add-author t)
  (:unbind "M-r")
  (:unbind "M-s"))

(setup log-view
  (:bind
   "<return>" #'log-view-find-revision
   "<tab>" #'log-view-toggle-entry-display
   "f" #'vc-log-incoming
   "F" #'vc-update
   "o" #'vc-log-outgoing
   "P" #'vc-push
   "s" #'vc-log-search))

;;;;; Diff

(setup ediff
  (setq!
   ediff-keep-variants nil
   ediff-make-buffers-readonly-at-startup nil
   ediff-show-clashes-only t)
  (setq! ediff-window-setup-function #'ediff-setup-windows-plain))

(setup diff
  (setq! diff-default-read-only t)
  (setq! diff-font-lock-prettify t
         diff-font-lock-syntax 'hunk-also))

(setup (:package diff-hl)
  (:with-mode global-diff-hl-mode
    (:hook-into after-init-hook))
  (if (display-graphic-p)
      (diff-hl-show-hunk-mouse-mode 1)
    (diff-hl-margin-mode 1))
  (:with-feature magit
    (:with-hook magit-pre-refresh-hook
      (:hook #'diff-hl-magit-pre-refresh))
    (:with-hook magit-post-refresh-hook
      (:hook #'diff-hl-magit-post-refresh)))
  (:with-feature dired
    (:hook #'diff-hl-dired-mode)))

;;;;; Git

(setup vc-git
  (setq! vc-git-diff-switches '( "--patch-with-stat"
                                 "--histogram"))
  (setq! vc-git-log-switches '("--stat"))
  (setq! vc-git-print-log-follow t)
  (setq! vc-git-revision-complete-only-branches t)
  ;; <https://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html>
  (setq! vc-git-log-edit-summary-target-len 50
         vc-git-log-edit-summary-max-len 72)
  (:with-map vc-git-stash-shared-map
    "A" #'vc-git-stash-apply-at-point
    "P" #'vc-git-stash-pop-at-point
    "z" #'vc-git-stash
    "Z" #'vc-git-stash-snapshot))

(setup (:package git-modes))

(setup (:package git-timemachine))

(setup (:package magit)
  (magit-wip-mode 1)
  (setq! magit-diff-refine-hunk t)
  (setq! magit-save-repository-buffers nil)
  (setq! magit-process-finish-apply-ansi-colors t)
  (setq! magit-process-popup-time 3)
  (:with-mode magit-status-mode
    (:bind "_" #'magit-revert)
    (:bind "x" #'magit-discard))
  (:with-feature nerd-icons
    (when (fboundp #'nerd-icons-insert)
      (setq! magit-format-file-function #'magit-format-file-nerd-icons)))
  (:when-loaded
    (transient-append-suffix 'magit-commit "-n"
      '("-S" "Disable GPG signing" "--no-gpg"))
    (transient-append-suffix 'magit-fetch "-p"
      '("-t" "Fetch all tags" ("-t" "--tags")))
    (transient-append-suffix 'magit-pull "-r"
      '("-a" "Autostash" "--autostash"))))


;;;;; Jujutsu (jj)

(setup (:package vc-jj)
  (require 'vc-jj))


;;;; Projects


;;;; Dired

(setup dired
  (:hook #'hl-line-mode)
  (:hook #'dired-omit-mode)
  ;; -A => dotfiles without . and ..
  ;; -F => append special chars to special files
  ;; -G => omit group name
  ;; -h => human-readable file sizes
  ;; -l => long listing, required by dired
  ;; -v => sort files by version number, not lexicographic
  (setq! dired-listing-switches
         "-AGFhlv --group-directories-first --time-style=long-iso")
  (setq! dired-auto-revert-buffer #'dired-directory-changed-p)
  (setq! dired-dwim-target t)
  (setq!
   dired-create-destination-dirs 'ask
   dired-create-destination-dirs-on-trailing-dirsep t
   dired-recursive-copies 'always
   dired-recursive-deletes 'always)
  (setq! dired-backup-overwrite 'always)
  (:with-feature mouse
    (setq! dired-make-directory-clickable t)
    (setq! dired-mouse-drag-files t))
  (:with-feature vc
    (setq! dired-vc-rename-file nil))
  (:bind "C-+" #'dired-create-empty-file
         "C-RET" #'dired-do-open
         "C-c C-e" #'wdired-change-to-wdired-mode))

(setup image-dired
  (:with-mode image-dired-thumbnail-mode
    (:bind "RET" #'image-dired-thumbnail-display-external))
  ;; (setq! image-dired-thumbnail-storage)
  )

(setup wdired
  (:bind "C-c C-k" #'wdired-change-to-dired-mode)
  (setq! wdired-create-parent-directories t)
  (setq! wdired-allow-to-change-permissions t))

(setup (:package dired-preview)
  (:with-mode dired-preview-global-mode
    (:hook-into after-init-hook)))

(setup (:package dired-subtree)
  (:with-feature dired
    (:bind
     "<tab>" #'dired-subtree-toggle
     "TAB" #'dired-subtree-toggle
     "<backtab>" #'dired-subtree-remove
     "S-TAB" #'dired-subtree-remove))
  (setq! dired-subtree-use-backgrounds nil))

(setup (:package diredfl)
  (:with-mode diredfl-global-mode
    (:hook-into after-init-hook))
  (:when-loaded
    (set-face-attribute 'diredfl-dir-name nil :bold t)))

(setup (:package nerd-icons-dired)
  (:hook-into dired-mode-hook))


;;;; Completion

(setup (:package orderless)
  (require 'orderless)
  (setq! completion-styles (append '(orderless) completion-styles))
  (setq! completion-category-overrides
         '((file . (styles . (partial-completion)))
           (bookmark . (styles . (basic substring)))
           (library . (styles . (basic substring)))
           (imenu . (styles . (orderless substring basic)))
           (kill-ring . (styles . (orderless))))))

(setup (:package consult)
  (:with-feature register
    (setq! register-preview-function #'consult-register-format
	   register-preview-delay 0.5)
    (advice-add #'register-preview :override #'consult-register-window))
  (:with-feature xref
    (setq! xref-show-definitions-function #'consult-xref
	   xref-show-xrefs-function #'consult-xref))
  (:with-feature pulsar
    (setq! consult-after-jump-hook nil)
    (:with-hook consult-after-jump-hook
      (:hook pulsar-recenter-top)
      (:hook pulsar-reveal-entry)))
  (:with-feature isearch
    (:with-map isearch-mode-map
      "M-e"   #'consult-isearch-history	; orig. `isearch-edit-string'
      "M-s e" #'consult-isearch-history	; orig. `isearch-edit-string'
      "M-s l" #'consult-line ; needed by `consult-line' to detect `isearch'
      "M-s L" #'consult-line-multi ; needed by `consult-line' to detect `isearch'
      ))
  (:with-feature info
    (keymap-global-set "<remap> <Info-search>" #'consult-info))
  (:with-feature embark
    (:with-map consult-narrow-map
      (:bind (concat consult-narrow-key " ?") #'embark-prefix-help-command)))
  (:with-map consult-narrow-map
    (:bind "?" #'consult-narrow-help))
  (:when-loaded
    (:with-feature consult-imenu
      (require 'consult-imenu)))
  (require 'consult)
  (setq! consult-narrow-key "<")
  (setq! consult-preview-key 'any)
  (consult-customize
   consult-theme consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

(setup (:package vertico)
  (:hook-into after-init-hook)
  (:with-hook rfn-eshadow-update-overlay-hook
    (:hook #'vertico-directory-tidy))
  (:with-feature savehist
    (:when-loaded
      (cl-pushnew #'vertico-repeat-history savehist-additional-variables)
      (:with-feature minibuffer
	(:with-hook minibuffer-setup-hook
	  (:hook #'vertico-repeat-save)))))
  (:with-feature vertico-multiform
    (:with-map vertico-multiform-map
      (:bind "C-l" #'vertico-multiform-vertical))
    (setq! vertico-multiform-commands
           `((consult-line buffer)
             (consult-imenu buffer)
             (consult-org-heading ,(lambda (_) (text-scale-set -1)))))
    (setq! vertico-multiform-categories
           '((buffer flat (vertico-cycle . t))
             (consult-grep buffer)
             (imenu (:not indexed mouse))
             (symbol (vertico-sort-function . vertico-sort-alpha))))
    (vertico-multiform-mode))
  (:with-map vertico-map
    (:bind "RET" #'vertico-directory-enter
	   "DEL" #'vertico-directory-delete-char
	   ;; TODO: prevent adding deletion to kill-ring
	   "M-DEL" #'vertico-directory-delete-word
	   "M-q" #'vertico-quick-insert
	   "C-j" #'vertico-insert
	   "C-q" #'vertico-quick-exit))
  (setq! vertico-count 8
	 vertico-cycle t
	 vertico-resize t))


;;;;; Minibuffer

(setup minibuffer
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)
  (file-name-shadow-mode 1)

  (setq! read-answer-short t)

  (setq! enable-recursive-minibuffers t)

  ;; Hide commands in M-x which do not apply to the current mode.
  (setq! read-extended-command-predicate #'command-completion-default-include-p)

  (setq! minibuffer-prompt-properties
	 '( read-only t
            cursor-intangible t
            face minibuffer-prompt))

  (setq! completion-ignore-case t
         read-buffer-completion-ignore-case t
         read-file-name-completion-ignore-case t)

  (:with-feature savehist
    (setq! savehist-save-minibuffer-history t)))

(setup (:package marginalia)
  (:hook-into after-init-hook)
  (:with-feature minibuffer
    (:with-map minibuffer-local-map
      (:bind "M-a" #'marginalia-cycle)))
  (setq! marginalia-align 'right))

(setup (:package nerd-icons-completion)
  (:with-feature marginalia
    (:hook #'nerd-icons-completion-marginalia-setup)
    (nerd-icons-completion-mode)))


;;;;; Completion-At-Point

(setup emacs
  (setq! tab-always-indent 'complete))

(setup (:package corfu)
  (:with-hook after-init-hook
    (:hook #'global-corfu-mode))
  (:with-feature savehist
    (:when-loaded
      (corfu-history-mode 1)
      (cl-pushnew 'corfu-history savehist-additional-variables)))
  (:with-feature minibuffer
    ;; Disable Corfu in the minibuffer when another completion UI is
    ;; active or when entering secrets.
    (setq! global-corfu-minibuffer
	   (lambda ()
             (not (or (bound-and-true-p mct--active)
                      (bound-and-true-p vertico--input)
                      (eq (current-local-map) read-passwd-map))))))
  (setq! corfu-preview-current t)
  (setq! corfu-popupinfo-delay '(1.0 . 0.5))
  (setq! corfu-cycle t)
  (corfu-popupinfo-mode 1))

(setup (:package kind-icon)
  (:with-feature corfu
    (setq! kind-icon-default-face 'corfu-default)
    (cl-pushnew #'kind-icon-margin-formatter corfu-margin-formatters))
  (:when-loaded
    (plist-put kind-icon-default-style :height 0.8)
    (:with-feature emacs
      (:with-hook enable-theme-functions
	(:hook (lambda (_) (kind-icon-reset-cache))))))
  (require 'kind-icon)
  (setq! kind-icon-use-icons (display-graphic-p))
  (setq! kind-icon-blend-background t))


;;;;; Embark

(setup (:package embark embark-consult)
  (:with-hook embark-collect-mode-hook
    (:hook #'consult-preview-at-point-mode))
  (:with-feature emacs
    (setq! prefix-help-command #'embark-prefix-help-command))
  (:with-feature vertico
    (cl-pushnew '(embark-keybinding grid) vertico-multiform-categories))
  (:with-feature mouse
    (:with-hook context-menu-functions
      ;; FIXME: unsupported
      ;; (:hook #'embark-context-menu 100)
      (add-hook 'context-menu-functions #'embark-context-menu 100)
      ))
  (cl-pushnew
   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
     nil
     (window-parameters (mode-line-format . none)))
   display-buffer-alist))


;;;; Files & Directories

(setup emacs
  (setq! create-lockfiles nil
         make-backup-files nil)
  (setq! delete-by-moving-to-trash t)
  (setq! find-file-suppress-same-file-warnings t
         find-file-visit-truename t)
  (:with-hook find-file-not-found-functions
    (:hook (defun ceamx-find-file-create-paths-h ()
             (unless (file-remote-p buffer-file-name)
               (let ((parent-dir (file-name-directory buffer-file-name)))
                 (and (not (file-directory-p parent-dir))
                      (y-or-n-p (format "Directory `%s' does not exist!  Create it?"
                                        parent-dir))
                      (progn (make-directory parent-dir 'parents)
                             t))))))))

;;;;; Auto-save file-visiting buffers

(setup emacs
  (setq! auto-save-interval 300
         auto-save-visited-interval 60
         auto-save-timeout 60
         ;; Don't create "~" auto-save files.
         auto-save-default nil)
  (auto-save-visited-mode))

;;;;; Remote Paths / TRAMP

(setup emacs
  (setq! remote-file-name-inhibit-auto-save t
         remote-file-name-inhibit-auto-save-visited t))


;;;; Snippets

(setup autoinsert
  (auto-insert-mode 1))

(setup (:package tempel)
  (setq! tempel-path (file-name-concat ceamx-templates-dir "tempel/*.eld"))
  (setq! tempel-trigger-prefix "<")
  (:with-map tempel-map
    "<tab>" #'tempel-next
    "<backtab>" #'tempel-previous))

(setup (:package yasnippet)
  (:with-hook after-init-hook
    (:hook #'yas-global-mode))
  (setq! yas-snippet-dirs (list (file-name-concat ceamx-templates-dir "yasnippet")))
  (setq! yas-prompt-functions '( yas-completing-prompt
                                 yas-no-prompt))
  (setq! yas-new-snippet-default
         "# -*- mode: snippet -*-\n# name: $1\n# key: ${2:${1:$(yas--key-from-desc yas-text)}}\n# uuid: `(uuidgen-4)`\n# contributor: astratagem <chmont@protonmail.com>\n# --\n$0`(yas-escape-text yas-selected-text)`")
  (:with-mode snippet-mode
    (:hook (defun ceamx+yasnippet-snippet-mode-load-deps-h ()
             (require 'uuidgen)))))

(setup (:package spdx))


;;;; Writing

(add-hook 'text-mode-hook
          (defun ceamx-text-modes-variable-pitch-h ()
            "Display text in variable-pitch fonts in text modes."
            (unless (derived-mode-p ceamx-text-mode-derived-prog-modes)
              (variable-pitch-mode 1))))

(setup (:package typo)
  (:with-hook text-mode-hook
    (:hook
     (defun ceamx+typo-mode-maybe-enable-h ()
       (let ((excluded-modes (append ceamx-text-mode-derived-prog-modes
                                     ceamx-typo-mode-excluded-modes)))
         (unless (derived-mode-p excluded-modes)
           (typo-mode 1)))))))

;; Install with system package manager due to dependencies.
(setup jinx
  (:hook-into text-mode)
  (setq! jinx-languages "en"))

;;;;; Markdown

(setup (:package markdown-mode)
  (setq! markdown-enable-wiki-links t)
  (setq! markdown-italic-underscore t)
  (setq! markdown-gfm-additional-languages '("sh"))
  (setq! markdown-fontify-whole-heading-line t)
  ;; HACK Due to jrblevin/markdown-mode#578, invoking `imenu' throws a
  ;;      'wrong-type-argument consp nil' error if you use native-comp.
  ;;      <https://github.com/jrblevin/markdown-mode/issues/578>
  (setq! markdown-nested-imenu-heading-index
         (not (ignore-errors (native-comp-available-p))))
  (setq! markdown-open-command "xdg-open")
  (:bind "C-c i l" #'markdown-insert-link
         "C-c i q" #'markdown-insert-blockquote)
  (:with-feature org-src
    (:when-loaded
      (cl-pushnew '("md" . markdown) org-src-lang-modes))))


;;;; Outline

(setup outline
  (setq! outline-minor-mode-highlight t)
  (setq! outline-minor-mode-cycle t)
  (setq! outline-minor-mode-use-buttons nil))

(setup (:package outli)
  (:bind
   "C-c C-n" #'outline-next-visible-heading
   "C-c C-p" #'outline-previous-visible-heading
   "C-c C-<" #'outline-promote
   "C-c C->" #'outline-demote
   "C-c C-u" #'outline-up-heading))


;;;; Structural Editing & Tree-Sitter

(setup treesit
  (setq! treesit-font-lock-level 4))

(setup (:package expreg))

(setup (:package puni)
  (puni-global-mode)
  (:with-hook term-mode-hook
    (:hook #'puni-disable-puni-mode)))

(setup (:package treesit-auto)
  (require 'treesit-auto)
  (setq! treesit-auto-install nil)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(setup (:package (combobulate :url "https://github.com/mickeynp/combobulate"))
  (:hook-into prog-mode-hook))


;;;; Folding

(setup (:package treesit-fold)
  (setq! treesit-fold-line-count-show t
         treesit-fold-summary-show nil)
  (global-treesit-fold-mode 1)
  (global-treesit-fold-indicators-mode 1))

(setup (:package savefold)
  (:hook-into after-init-hook)
  (setq! savefold-directory (file-name-concat ceamx-cache-dir "savefold"))
  (setq! savefold-backends '(hideshow outline))
  (:with-feature org
    (:when-loaded
      (cl-pushnew 'org savefold-backends)))
  (:with-feature treesit-fold
    (:when-loaded
      (cl-pushnew 'treesit-fold savefold-backends))))


;;;; Org-Mode

(setq! ceamx-default-agenda-files
       (list (file-name-concat ceamx-agenda-dir "todo.org")
             (file-name-concat ceamx-agenda-dir "work.org")))

(setup org
  (progn
    (setq! org-directory ceamx-agenda-dir)
    (make-directory org-directory t)))

(setup (:package doct)
  (require 'doct))

(setup (:package org-ql)
  (require 'org-ql))


;;;;; Org-Mode: Links & IDs

(setup org
  (setq! org-agenda-files ceamx-default-agenda-files)
  (setq! org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (setq! org-clone-delete-id t))

;;;;; Org-Mode: Editing

(setup org
  (setq! org-return-follows-link t)
  (setq! org-special-ctrl-a/e t
         org-special-ctrl-k t
         org-ctrl-k-protect-subtree t)
  (setq! org-list-use-circular-motion t)
  (setq! org-M-RET-may-split-line '((default . nil))
         org-insert-heading-respect-content nil))

;;;;; Org-Mode: Folding

(setup org
  (setq! org-fold-catch-invisible-edits 'show-and-error)
  (:with-feature savefold
    (when (bound-and-true-p savefold-mode)
      (setq! org-startup-folded 'showeverything))))

;;;;; Org-Mode: Priority

(setup org
  (setq! org-priority-start-cycle-with-default nil))

;;;;; Org-Mode: Workflow states

(setup org
  (setq! org-enforce-todo-dependencies t
         org-enforce-todo-checkbox-dependencies t)
  (setq! org-todo-keywords
         '((sequence
            "TODO(t)"
            "INPRG(i@/!)"
            "BLOCKED(b@)"
            "HOLD(h@)"
            "PROJ(p)"
            "|"
            "DONE(d!)"
            "CANCELLED(x@/!)")))
  (setq! org-clock-in-switch-to-state "INPRG"))

;;;;; Org-Mode: Logbook

(setup org
  (setq! org-log-done 'time
         org-log-redeadline 'time
         org-log-refile 'time)
  (setq! org-log-into-drawer t)
  (setq! org-log-states-order-reversed nil))

;;;;; Org-Mode: Media & attachments

(setup org
  (setq! org-image-actual-width 480)
  (setq! org-startup-with-inline-images t))

(setup (:package org-download)
  (:with-feature dired
    (:hook #'org-download-enable)))

(setup (:package org-web-tools))

;;;;; Org-Mode: Appearance

(setup org
  (:hook #'prettify-symbols-mode)
  ;; (setq! org-auto-align-tags nil
  ;;        org-tags-column 0
  ;;        org-agenda-tags-column 0)
  (setq! org-pretty-entities t))

(setup (:package org-modern)
  (:hook-into org-mode-hook)
  (:with-feature org-agenda
    (:with-hook org-agenda-finalize-hook
      (:hook #'org-modern-agenda)))
  (setq! org-modern-checkbox nil
         org-modern-internal-target nil
         org-modern-keyword t
         org-modern-priority t
         org-modern-radio-target nil
         org-modern-star 'replace
         org-modern-todo t
         org-modern-table nil)
  ;; (setq! org-modern-replace-stars "⦿")
  ;; (setq! org-modern-replace-stars "│")
  (setq! org-modern-replace-stars "├")
  (setq! org-modern-hide-stars "│")
  ;; (setq! org-modern-hide-stars "⎸")
  ;; (setq! org-modern-hide-stars
  ;;        (propertize "┄" 'face 'modus-themes-fg-cyan-faint))
  )

(setup (:package org-appear)
  (:hook-into org-mode-hook)
  (setq! org-appear-autolinks t
         org-appear-autoentities t
         org-appear-autokeywords t
         org-appear-inside-latex t)
  (setq! org-appear-delay 0.3)
  (setq! org-appear-trigger 'always))

(setup org
  (:with-feature pulsar
    (:with-hook ( org-agenda-after-show-hook
                  org-follow-link-hook)
      (:hook #'pulsar-recenter-center
             #'pulsar-reveal-entry))))

;;;;; Org-Mode: Indentation

(setup org
  (setq! org-indent-indentation-per-level 2))

;;;;; Org-Mode: Navigation

(setup org-goto
  (setq! org-goto-interface 'outline-path-completion))

(defvar-keymap org-navigation-repeat-map
  :repeat t
  "C-b" #'org-backward-heading-same-level
  "b" #'org-backward-heading-same-level
  "C-f" #'org-forward-heading-same-level
  "f" #'org-forward-heading-same-level
  "C-n" #'org-next-visible-heading
  "n" #'org-next-visible-heading
  "C-p" #'org-previous-visible-heading
  "p" #'org-previous-visible-heading
  "C-u" #'org-up-heading
  "u" #'org-up-heading
  "C-<" #'org-promote-subtree
  "<" #'org-promote-subtree
  "C->" #'org-demote-subtree
  ">" #'org-demote-subtree)

;;;;; Org-Mode: Refiling

(setup org-refile
  (setq! org-outline-path-complete-in-steps nil)
  (setq! org-refile-use-outline-path 'file)
  (setq! org-refile-allow-creating-parent-nodes 'confirm)
  (setq! org-refile-use-cache nil)
  (setq! org-refile-targets `((,ceamx-default-todo-file . (:level 1))
                              (nil . (:maxlevel . ,ceamx-outline-search-max-level)))))

;;;;; Org-Mode: Archive

(setup org-archive
  (setq! org-archive-save-context-info
         '(time file category todo itags olpath ltags)))

;;;;; Org-Mode: Agenda

(setup org-agenda
  (setq! org-agenda-time-grid
         '((daily today require-timed)
           (800 1000 1200 1400 1600 1800 2000)
           " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (setq! org-agenda-current-time-string
         "⭠ now ─────────────────────────────────────────────────"))

(setup (:package org-super-agenda))

;;;;; Org-Mode: Literate

(setup org-src
  (setq! org-edit-src-content-indentation 0
         org-src-preserve-indentation t)
  (setq! org-edit-src-persistent-message nil
         org-src-ask-before-returning-to-edit-buffer nil)
  (setq! org-src-tab-acts-natively t)
  (setq! org-src-window-setup 'current-window))

;;;;; Org-Mode: Sidebar & Annotations

(setup (:package org-sidebar))

(setup (:package org-remark))

;;;;; Org-Mode: Export

(setup (:package ox-gfm)
  (:with-feature org
    (:when-loaded
      (require 'ox-gfm))))

;;;;; Org-Mode: Tangle

(setup (:package (auto-tangle-mode :url "https://github.com/progfolio/auto-tangle-mode.el"))
  (:with-feature minions
    (:when-loaded
      (cl-pushnew #'auto-tangle-mode minions-prominent-modes))))

;;;; Programming Modes

(require 'ceamx-prog)

(setup (:package dumb-jump)
  (:with-feature xref
    (:when-loaded
      (setq-default xref-backend-functions
                    (append xref-backend-functions '(dumb-jump-xref-activate))))))

;; Colorize color names and hexcodes in buffers.
(setup (:package rainbow-mode))

(setup (:package hl-todo)
  (:hook-into prog-mode-hook))

(setup (:package indent-bars)
  (:hook-into python-base-mode-hook yaml-mode-hook yaml-ts-mode-hook)
  (setq! indent-bars-no-descend-lists t)
  (setq! indent-bars-treesit-support t)
  (setq! indent-bars-treesit-ignore-blank-lines-types '("module")))

;;;;; Formatters

(setup (:package reformatter)
  (require 'reformatter))

;;;;;; Formatter: prettier

(setup reformatter
  (:when-loaded
    (reformatter-define prettier
      :group 'ceamx
      :program "prettier"
      :args (list (concat "--plugin-search-dir="
                          (expand-file-name
                           (locate-dominating-file default-directory "package.json")))
                  "--stdin-filepath" (buffer-file-name)))))

;;;;;; Formatter: treefmt

(setup reformatter
  (:when-loaded
    (reformatter-define treefmt
      :group 'ceamx
      :program "treefmt"
      :args (list "--stdin" (buffer-file-name)))))

;;;;;; Formatter: biome

(setup reformatter
  (:when-loaded
    (reformatter-define biome-format
      :group 'ceamx
      :program "biome"
      :args (list "format" "--stdin-file-path" (buffer-file-name)))
    (dolist (hook (ceamx-prog-biome-supported-modes-hooks))
      (add-hook hook #'biome-format-on-save-mode))))

;;;;; Flymake

(setup flymake
  (setq! flymake-fringe-indicator-position 'right-fringe)
  (setq! flymake-no-changes-timeout 1.0))

;;;;; Flycheck

(setup (:package flycheck)
  (:with-mode global-flycheck-mode
    (:hook-into after-init-hook))
  (setq! flycheck-emacs-lisp-load-path 'inherit)
  (setq! flycheck-idle-change-delay 1.0)
  (setq! flycheck-display-errors-delay 1.5)
  (setq! flycheck-check-syntax-automatically
         '(save idle-change mode-enabled))
  (setq! flycheck-buffer-switch-check-intermediate-buffers nil)
  (:when-loaded
    ;; Disable Flycheck for modes supported by Flymake
    (setq-default flycheck-disabled-checkers
                  (append (default-value 'flycheck-disabled-checkers)
                          '( emacs-lisp
                             emacs-lisp-checkdoc
                             emacs-lisp-package
                             sh-shellcheck)))))

;;;;; Eglot

(require 'ceamx-eglot)

(setup eglot
  (add-hook 'prog-mode-hook #'eglot-ensure)
  (:when-loaded
    (advice-add
     #'eglot-ensure :around
     (defun ceamx+eglot-ensure-available-mode-a (fn)
       "Gate ‘eglot-ensure’ to supported modes only."
       (when (alist-get major-mode eglot-server-programs nil nil
                        (lambda (modes key)
                          (if (listp modes) (member key modes) (eq key modes))))
         (funcall fn)))))
  (:with-feature popper
    (:when-loaded
      (cl-pushnew "^\\*eglot-help" popper-reference-buffers))))

(setup (:package flycheck-eglot)
  (:hook-into eglot-managed-mode-hook))

(setup (:package consult-eglot))

;;;;; All Lisps

(setup emacs
  (setq-default lisp-indent-offset nil))

(setup ceamx-lisp
  (:with-mode ceamx-lisp-global-mode
    (:hook-into after-init-hook)))

(setup (:package lispy)
  (:bind "`" #'self-insert-command)
  (:unbind "M-j" "M-o" "`" "'")
  (setq! lispy-completion-method 'default)
  (setq! lispy-eval-display-style 'message)
  (:with-feature macrostep
    (:when-loaded
      (push 'macrostep lispy-compat)))
  (:with-feature popper
    (:when-loaded
      ;; FIXME: somehow this can get added multiple times... but how? a bug?
      (cl-pushnew "\\*lispy-message\\*" popper-reference-buffers))))

;;;;; Emacs Lisp

(setup elisp-mode
  (:with-mode emacs-lisp-mode
    (:bind ceamx-repl-key #'ielm)))

(setup (:package eros)
  (:hook-into emacs-lisp-mode-hook)
  (:with-mode emacs-lisp-mode
    (:bind "<remap> <eval-last-sexp>" #'eros-eval-last-sexp))
  (:with-feature lispy
    (:hook (defun ceamx+eros+lispy-use-eros-eval-h ()
             (lispy-define-key lispy-mode-map "e" #'eros-eval-last-sexp)))))

(setup (:package macrostep))

(setup (:package morlock)
  (:hook-into after-init-hook))

(setup (:package keymap-utils))

;;;;; `kbd-mode'

(setup (:package (kanata-kbd-mode :url "https://github.com/chmouel/kanata-kbd-mode"))
  ;; TODO: hopefully this is not necessary
  ;; (:match-file "\\.kbd\\'")
  (:with-feature aggressive-indent
    (:when-loaded
      (cl-pushnew 'kanata-kbd-mode aggressive-indent-excluded-modes))))

;;;;; JSON

(setup json-ts-mode
  (:file-match "\\.jsonc\\'"))

;;;;; TOML

(setup reformatter
  (:when-loaded
    (reformatter-define toml-taplo-fmt
      :group 'ceamx
      :program "taplo"
      :args (list "format" "--diff"
                  "--stdin-filepath" (buffer-file-name)
                  "-"))
    (:with-mode toml-taplo-fmt-on-save-mode
      (:hook-into conf-toml-mode-hook toml-ts-mode-hook))))

(setup eglot
  (:when-loaded
    (:with-feature ceamx-eglot
      (cl-pushnew '("toml-taplo" . nil) ceamx-eglot-server-configurations-alist)
      (cl-pushnew (cons '(conf-toml-mode toml-ts-mode)
                        (ceamx-eglot-server-contact "toml-taplo"
                                                    "taplo" "lsp" "stdio"))
                  eglot-server-programs))))

;;;;; YAML (is a terrible)

(setup yaml-ts-mode
  (:hook (defun ceamx+yaml-ts-mode-ensure-indentation ()
           (setq-local tab-indent 2)
           (setq-local indent-bars-offset 2))))

(setup (:package yaml-pro)
  (add-hook 'yaml-ts-mode-hook #'yaml-pro-ts-mode 100))

;;;;; XML

(setup nxml-mode
  (setq! nxml-slash-auto-complete-flag t)
  (setq! nxml-auto-insert-xml-declaration-flag t))

;;;;; KDL

(setup (:package (kdl-ts-mode :url "https://github.com/merrickluo/kdl-ts-mode"))
  (:file-match "\\.kdl\\'"))

;;;;; jq

(setup (:package jq-mode)
  (:file-match "\\.jq$")
  (:hook (lambda () (electric-pair-local-mode -1)))
  (:with-feature json-ts-mode
    (:bind ceamx-repl-key #'jq-interactively)))

;;;;; JavaScript

(setup typescript-ts-mode
  (:when-loaded
    (setq-default js-indent-level 2)))

;;;;; Rust

(setup (:package rust-mode))

(setup (:package rustic))

;;;;; Lua

(setup lua-mode
  (:when-loaded
    (setq-default lua-indent-level 4)))

;;;;; Nix

(setup (:package nix-ts-mode)
  (:hook (defun ceamx+nix-ts-mode-insert-semicolons-h ()
           (add-hook 'post-self-insert-hook #'ceamx-prog-nix-insert-semicolon-after-sequence nil t)))
  (:bind ceamx-repl-key #'nix-repl)
  (:with-feature nix-repl
    (:bind ceamx-repl-key #'quit-window))
  (:with-feature aggressive-indent
    (:when-loaded
      (cl-pushnew 'nix-ts-mode aggressive-indent-excluded-modes))))

(setup reformatter
  (:when-loaded
    (reformatter-define nixfmt-format
      :group 'ceamx
      :program "nixfmt")
    (:with-mode nixfmt-format-on-save-mode
      (:hook-into nix-ts-mode-hook))))

;;;;; Web Development

(setup (:package web-mode)
  ;; Defer to ‘electric-pair-mode’.
  (setq! web-mode-enable-auto-pairing nil)
  (when (package-installed-p 'prism)
    (setq! web-mode-enable-css-colorization nil))
  (setq! web-mode-enable-block-face t)
  (setq! web-mode-enable-part-face t)
  (setq! web-mode-enable-current-element-highlight t))

(setup (:package emmet-mode)
  (:hook-into css-mode-hook web-mode-hook)
  (setq! emmet-move-cursor-between-quotes t))

;;;;; PHP

(setup php-ts-mode
  (:hook #'display-line-numbers-mode))

(setup (:package neon-mode))

(setup (:package flycheck-phpstan)
  (:with-feature php-ts-mode
    (:hook (defun ceamx+flycheck-phpstan-load-h ()
             (require 'flycheck-phpstan)))))

;;;;; Shell Scripts

(setup eglot
  (:when-loaded
    (cl-pushnew '((sh-mode bash-ts-mode) . ("bash-language-server" "start"))
                eglot-server-programs)))

(setup sh-script
  (:with-mode ( sh-mode bash-ts-mode)
    (:hook #'flymake-mode)))

(setup (:package fish-mode)
  (setq! fish-indent-offset 4)
  (:with-feature eglot
    (:when-loaded
      (cl-pushnew '(fish-mode . ("fish-lsp" "start")) eglot-server-programs))))

;;;;; Just

(setup (:package just-ts-mode)
  (:file-match "\\.just\\'")
  (:with-feature ceamx-eglot
    (:when-loaded
      (cl-pushnew '("just-just-lsp" . nil)
                  ceamx-eglot-server-configurations-alist)
      (:with-feature eglot
        (:when-loaded
          (cl-pushnew `(just-ts-mode . ,(ceamx-eglot-server-contact "just-just-lsp"))
                      eglot-server-programs))))))

;;;;; Dotenv

(setup (:package dotenv-mode))

;;;;; yuck

(setup (:package yuck-mode))


;;;; Notes

(require 'ceamx-paths)

(setup ceamx-note
  (require 'ceamx-note))

(setup (:package org-mem)
  (setq! org-mem-do-sync-with-org-id t)
  (org-mem-updater-mode 1))

(setup (:package org-node)
  (require 'org-node)
  (setq! org-node-extra-id-dirs (list ceamx-agenda-dir))
  (cl-pushnew "journal" org-node-extra-id-dirs-exclude)
  (org-node-backlink-mode 1)
  (org-node-cache-mode 1))

;;;; Presentation

(setup (:package keycast))


;;;; PDF-Tools

;; Package installed via Nixpkgs due to dependencies.
(setup pdf-tools
  (:with-mode pdf-view-mode
    (:file-match "\\.pdf\\'")))

;;;; News

(setup ceamx-news
  (require 'ceamx-news))

(setup (:package elfeed)
  (:when-loaded
    (unless (file-exists-p elfeed-db-directory)
      (make-directory elfeed-db-directory t)))
  (setq! elfeed-search-filter "@1-week-ago +unread")
  (:with-feature ceamx-news
    (setq! elfeed-show-entry-switch #'ceamx-news/open-entry
           elfeed-show-entry-delete #'ceamx-news/delete-pane))
  (:with-feature popper
    (:when-loaded
      (cl-pushnew "^\\*elfeed-entry" popper-reference-buffers))))

(setup (:package elfeed-goodies)
  (:with-feature elfeed
    (:when-loaded
      (require 'elfeed-goodies)
      (elfeed-goodies/setup))))

(setup (:package elfeed-org)
  ;; Must be set prior to feature load.
  (setq! rmh-elfeed-org-files
         (list (file-name-concat ceamx-feeds-dir "index.org")))
  (:with-feature elfeed
    (:when-loaded
      (require 'elfeed-org)
      (elfeed-org))))

(setup (:package elfeed-tube)
  (:with-feature elfeed
    (:when-loaded
      (require 'elfeed-tube)
      (setq! elfeed-tube-auto-save-p nil
             elfeed-tube-auto-fetch-p t))
    (:with-mode ( elfeed-show-mode elfeed-search-mode)
      (:bind "F" #'elfeed-tube-fetch
             "<remap> <save-buffer>" #'elfeed-tube-save))))

(setup (:package elfeed-tube-mpv)
  (:with-feature elfeed
    (:with-mode elfeed-show-mode
      (:bind "C-c C-f" #'elfeed-tube-mpv-follow-mode
             "C-c C-w" #'elfeed-tube-mpv-where))))

(setup (:package elfeed-score)
  (:with-feature elfeed
    (:when-loaded
      (require 'elfeed-score)
      (elfeed-score-enable)
      (:with-mode elfeed-search-mode
        (:bind "=" elfeed-score-map))))
  (setq! elfeed-score-serde-score-file
         (file-name-concat ceamx-feeds-dir "elfeed-scores.eld")))

;;;; Mail

(setup (:package notmuch)
  (setq! notmuch-show-logo nil
         notmuch-column-control 1.0
         notmuch-hello-auto-refresh t
         notmuch-hello-recent-searches-max 20
         notmuch-hello-thousands-separator ""
         ;; notmuch-hello-sections '(notmuch-hello-insert-saved-searches)
         notmuch-show-all-tags-list t))

(setup (:package notmuch-indicator)
  (setq! notmuch-indicator-args
         '(( :terms "tag:unread and tag:inbox"
             :label "[U] ")))
  (setq! notmuch-indicator-refresh-count (* 60 3))
  (setq! notmuch-indicator-hide-empty-counters t)
  (setq! notmuch-indicator-force-refresh-commands
         '(notmuch-refresh-this-buffer))
  (notmuch-indicator-mode 1))

;;;;; Mail: Security

(setup emacs
  (setq! mml-secure-openpgp-encrypt-to-self t
         mml-secure-openpgp-sign-with-sender t)
  (setq! mml-secure-smime-encrypt-to-self t
         mml-secure-smime-sign-with-sender t))

;;;;; Mail: Search

(setup notmuch
  (setq! notmuch-search-oldest-first nil)
  (setq! notmuch-show-empty-saved-searches t)
  (setq! notmuch-saved-searches
         '(( :name "inbox"
             :query "tag:inbox"
             :sort-order newest-first
             :key "i")
           ( :name "unread"
             :query "tag:unread and tag:inbox"
             :sort-order newest-first
             :key "u")))
  (:with-mode notmuch-search-mode
    (:bind "/" #'notmuch-search-filter
           "r" #'notmuch-search-reply-to-thread
           "R" #'notmuch-search-reply-to-thread-sender)))

(setup (:package consult-notmuch))

;;;;; Mail: Compose

(setup message
  (:with-hook message-setup-hook
    (:hook #'message-sort-headers))
  (setq! mail-user-agent 'message-user-agent
         message-mail-user-agent t)
  (setq! mail-header-separator "-- text follows this line --")
  (setq! message-elide-ellipsis "\n> [... %l lines elided]\n")
  (setq! compose-mail-user-agent-warnings nil)
  (setq! message-signature "Chris Montgomery"
         mail-signature message-signature)
  (setq! message-citation-line-function #'message-insert-formatted-citation-line)
  (setq! message-citation-line-format (concat "> From: %f\n"
                                              "> Date: %a, %e %b %Y %T %z\n"
                                              ">")
         message-ignored-cited-headers "")
  (setq! message-confirm-send t)
  (setq! message-kill-buffer-on-exit t)
  ;; Always reply-all.
  (setq! message-wide-reply-confirm-recipients nil))

(setup notmuch
  (setq! notmuch-mua-compose-in 'current-window)
  (setq! notmuch-mua-hidden-headers nil)
  (setq! notmuch-address-command 'internal)
  (setq! notmuch-address-use-company nil)
  (setq! notmuch-always-prompt-for-sender t)
  (setq! notmuch-mua-cite-function #'message-cite-original-without-signature)
  (setq! notmuch-mua-reply-insert-header-p-function
         #'notmuch-show-reply-insert-header-p-never)
  (setq! notmuch-mua-user-agent-function nil)
  (setq! notmuch-maildir-use-notmuch-insert t)
  (setq! notmuch-crypto-process-mime t
         notmuch-crypto-get-keys-asynchronously t)
  (:with-mode notmuch-show-mode
    (:bind "r" #'notmuch-search-reply-to-thread
           "R" #'notmuch-search-reply-to-thread-sender)))

;;;;; Mail: Attachments

(setup gnus
  (:with-feature dired
    (:hook #'turn-on-gnus-dired-mode)))

;;;;; Mail: Reading Messages

(setup notmuch
  (setq! notmuch-show-relative-dates t)
  (setq! notmuch-show-all-multipart/alternative-parts nil)
  (setq! notmuch-show-indent-messages-width 1
         notmuch-show-indent-multipart nil)
  (setq! notmuch-show-part-button-default-action
         #'notmuch-show-view-part) ; orig. `notmuch-show-save-part'
  (setq! notmuch-wash-wrap-lines-length 120)
  (setq! notmuch-unthreaded-show-out nil)
  (setq! notmuch-message-headers '("To" "Cc" "Subject" "Date")
         notmuch-message-headers-visible t)

  ;; Disable buttonisation of long quotes.
  (let ((count most-positive-fixnum))
    (setq! notmuch-wash-citation-lines-prefix count
           notmuch-wash-citation-lines-suffix count)))

;;;; Web

(setup shr
  (setq! shr-max-width 90)
  ;; Usually color rendering does not work out so well.  Email messages,
  ;; which unfortunately are often HTML, have big blocks of color making
  ;; text difficult to read.  Ideally, colors would be applied in a more
  ;; nuanced manner, but for now, just disable.
  (setq! shr-use-colors nil)
  (setq! shr-folding-mode t)
  (setq! shr-bullet "• "))

(setup eww
  (:bind "," #'scroll-up-command
         "." #'scroll-down-command
         "o" #'link-hint-open-link))

;;;; Shells & Terminal Emulation

(setup eshell
  (setq! eshell-scroll-to-bottom-on-input 'this))

(setup (:package eat)
  (:with-feature eshell
    (:with-hook eshell-load-hook
      (:hook #'eat-eshell-mode)
      (:hook #'eat-eshell-visual-command-mode)))
  (:with-feature popper
    (:when-loaded
      (cl-pushnew "\\*eat\\*" popper-reference-buffers))))

;;;; Tools

;; Make HTTP requests in Org-Mode.
(setup (:package verb))

;; A QMK configurator as a Lisp.
(setup (:package mugur))

;; Show free keybindings for mod-keys or prefixes.
(setup (:package free-keys))

;; Keybinding introspection.
(setup (:package help-find))

;;;; Export

(setup (:package pandoc-mode)
  (:hook #'pandoc-load-default-settings)
  (:with-feature markdown-mode
    (:hook #'pandoc-mode)))

(setup (:package htmlize))

;;;; AI

(require 'ceamx-ai)

(setup (:package gptel)
  (setq! gptel-model 'claude-sonnet-4-20250514)
  (setq! gptel-backend
         (gptel-make-anthropic "Claude"
           :stream t
           :key (## ceamx-auth/lookup "api.anthropic.com" "emacs-gptel"))))

;;;; Capture

(setup org-capture
  (require 'org-capture)
  (setq! org-capture-templates
         (doct
          `(("Inbox item" :keys "c"
             :file ceamx-default-todo-file
             :headline "Inbox"
             :template ("* TODO %?"
                        "%i %a")
             :icon ("checklist" :set "octicon" :color "green"))
            ;; ("Journal entry" :keys "j"
            ;;  :file denote-)
            ))))


;;;; Keybindings

(define-keymap :keymap global-map
  ;; "ESC ESC" #'ceamx/keyboard-quit-dwim

  "C-a" #'mwim-beginning
  ;; "C-g" #'ceamx/keyboard-quit-dwim
  "C-e" #'mwim-end
  "C-h" help-map

  "S-RET" #'crux-smart-open-line)

;;;;; [C-*]

(define-keymap :keymap global-map
  "C-`" #'popper-toggle
  "C-~" #'popper-cycle
  "C-M-`" #'popper-toggle-type
  "C-^" #'crux-top-join-line
  "C-+" #'expreg-expand
  "C-=" #'expreg-contract
  "C-;" #'avy-goto-char-timer
  ;; "C-'" :: RESERVED: special commands e.g. `avy-org-goto-heading-timer'
  "C-." #'embark-act
  ;; "C-<" #'ceamx-simple/escape-url-dwim

  "C-RET" #'ceamx-simple/new-line-below
  "C-S-RET" #'crux-smart-open-line-above

  ;; "C-M-SPC" #'puni-mark-sexp-at-point
  ;; "C-M-@" #'easy-mark
  "C-M-#" #'consult-register
  ;; "C-M-$" #'jinx-languages

  "C-S-d" #'crux-duplicate-current-line-or-region
  "C-M-S-d" #'crux-duplicate-and-comment-current-line-or-region

  ;; "C-k" #'crux-smart-kill-line

  ;; TODO: redundant with `easy-kill'
  ;; "C-S-w" #'ceamx-simple/copy-line

  ;; "C-S-y" #'ceamx-simple/yank-replace-line-or-region
  )

;;;;; [C-h] :: Help Map

(define-keymap :keymap (current-global-map)
  "C-h b" #'embark-bindings
  "C-h B" #'describe-bindings
  "C-h c" #'helpful-callable
  "C-h C" #'helpful-command
  ;; "C-h D" #'devdocs-lookup
  "C-h f" #'helpful-function
  "C-h F" #'describe-face
  ;; FIXME: conflict
  ;; "C-h F" #'apropos-function
  "C-h h" #'helpful-at-point
  "C-h i" (cons "[ INFO ]" #'ceamx-info-prefix)
  ;; "C-h i i" #'ceamx/consult-info-dwim
  ;; "C-h i c" #'ceamx/completion-info
  ;; "C-h i e" #'ceamx/emacs-info
  ;; "C-h i o" #'ceamx/org-info
  "C-h I" #'consult-info
  "C-h k" #'helpful-key
  "C-h K" (cons "[ KEYBINDS ]" #'ceamx-help-keybindings-prefix)
  "C-h K b" #'help-find-keybinding
  "C-h K f" #'help-find-function
  "C-h K K" #'helpful-key
  "C-h l" #'find-library
  "C-h L" #'apropos-library
  "C-h m" #'describe-mode
  "C-h M" #'consult-man
  "C-h o" #'helpful-symbol
  "C-h t" #'describe-text-properties
  "C-h U" #'apropos-user-option
  "C-h v" #'helpful-variable
  "C-h V" #'apropos-variable)

;;;;; [C-c] :: User Prefix

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

;;;;; [C-c !] :: CHECKS

(define-keymap :keymap global-map
  "C-c ! l" #'flymake-show-buffer-diagnostics
  "C-c ! n" #'flymake-goto-next-error
  "C-c ! p" #'flymake-goto-previous-error
  "C-c ! c" #'flymake-show-buffer-diagnostics)

;;;;; [C-c b] :: BUFFER

(define-keymap :keymap ceamx-buffer-prefix
  "e" (cons "[ EXPORT ]" #'ceamx-export-prefix)
  "e h" #'htmlize-buffer)

;;;;; [C-c c] :: CAPTURE

(define-keymap :keymap ceamx-capture-prefix
  ;; TODO: <https://protesilaos.com/emacs/denote#text-h:eb72086e-05be-4ae3-af51-7616999fc7c9>
  "r" #'denote-region)

;;;;; [C-c h] :: HISTORY

(define-keymap :keymap ceamx-history-prefix
  "f" #'consult-recent-file
  "h" #'consult-history)

;;;;; [C-c i] :: INSERT

(define-keymap :keymap ceamx-insert-prefix
  "c" #'nerd-icons-insert
  "d" #'ceamx-simple/insert-date
  "i" #'yas-insert-snippet
  "l" nil                               ; RESERVED: insert link
  "L" #'spdx-insert-spdx
  "n" #'org-node-insert-link
  "q" nil                               ; RESERVED: insert quote
  "u" #'uuidgen-4)

(setup org-web-tools
  (:with-feature org
    (:bind "C-c i l" #'org-web-tools-insert-link-for-url)))

;;;;; [C-c f] :: FILE

(define-keymap :keymap ceamx-file-prefix
  "c" '("copy..." . crux-copy-file-preserve-attributes)
  ;; "d" '("delete" . ceamx-simple/delete-current-file)
  "d" '("delete" . crux-delete-file-and-buffer)
  "f" #'find-file
  "F" #'find-file-other-window
  "r" '("move..." . crux-rename-file-and-buffer))

;;;;; [C-c l] :: LANG / CODE

(define-keymap :keymap ceamx-code-prefix
  "." #'xref-find-definitions
  "f" (cons "[ FMT  ]" (define-prefix-command 'ceamx-code-F-prefix))
  ;; "j" #'ceamx/dumb-jump-dispatch/body
  "l" nil                            ; RESERVED: language-specific
  "o" nil                            ; RESERVED: language server symbols
  )

(setup eglot
  (:bind "C-c l a" #'eglot-code-actions
         "C-c l l" (define-prefix-command 'ceamx-lang-specific-prefix)
         "C-c l o" #'consult-eglot-symbols
         "C-c l r" #'eglot-rename))

(setup csv-mode
  (:bind "C-c l l a" #'csv-align-fields
         "C-c l l u" #'csv-unalign-fields
         "C-c l l s" #'csv-sort-fields
         "C-c l l S" #'csv-sort-numeric-fields
         "C-c l l k" #'csv-kill-fields
         "C-c l l t" #'csv-transpose))

;;;;; [C-c m] :: BOOKMARK

(define-keymap :keymap ceamx-bookmark-prefix
  "b" #'bookmark-in-project-jump
  "m" #'consult-bookmark
  "n" #'bookmark-in-project-jump-next
  "p" #'bookmark-in-project-jump-previous
  "*" #'bookmark-in-project-toggle)

;;;;; [C-c n] :: NOTE

(define-keymap :keymap ceamx-note-prefix
  "o" nil                               ; RESERVED: for `org-node'
  )

(setup org-node
  (:when-loaded
    (keymap-global-set "C-c n o" org-node-global-prefix-map)
    (:with-feature org
      (:bind "C-c n o" org-node-org-prefix-map))))

;;;;; [C-c o] :: LAUNCH

(define-keymap :keymap ceamx-launch-prefix
  "a" #'org-agenda
  "b" #'eww
  "f" #'elfeed
  "m" #'notmuch
  "s" #'scratch-buffer
  "t" #'eat
  "W" #'ceamx/eww-wiki)

;;;;; [C-c q] :: SESSION

(define-keymap :keymap ceamx-session-prefix
  "a c" #'cursory-set-preset
  "a d" #'ceamx-ui/dark
  "a f" #'fontaine-set-preset
  "a l" #'ceamx-ui/light
  "a t" #'consult-theme
  "a o" #'olivetti-mode

  "p" (cons "[ PACKAGES ]" (define-prefix-command 'ceamx-session-p-prefix))
  "p l" #'list-packages
  "p r" #'package-reinstall
  "p t" #'package-install
  "p u" #'package-upgrade-all

  "q" #'save-buffers-kill-emacs
  "Q" #'kill-emacs
  "r" #'restart-emacs)

;; FIXME: update for package.el
;; (define-keymap :keymap ceamx-session-p-prefix
;;   "f" #'elpaca-fetch-all
;;   "F" #'elpaca-fetch
;;   "i" #'elpaca-info
;;   "m" #'elpaca-merge-all
;;   "p" #'elpaca-pull
;;   "r" #'+elpaca-reload-package
;;   "t" #'elpaca-try
;;   "u" #'elpaca-update)

;;;;; [C-c t] :: TOGGLE

(define-keymap :keymap ceamx-toggle-prefix
  "c" (cons "cycle..." (define-prefix-command 'ceamx-toggle-c-prefix))
  "c c" #'cycle-at-point
  "c s" #'string-inflection-toggle
  "c +" #'shift-number-up
  "c -" #'shift-number-down
  "i" nil                                 ; RESERVED: for "images"
  "f" #'flycheck-mode
  "k" #'keycast-mode-line-mode
  "l" #'display-line-numbers-mode
  "M" #'menu-bar-mode
  "o" #'outline-minor-mode
  "p" nil                               ; RESERVED: for "previews"
  "s" #'jinx-mode
  "t" #'typo-mode
  "T" #'tab-bar-mode
  "w" #'window-toggle-side-windows
  "W" #'toggle-window-dedicated
  "z" #'logos-focus-mode
  "Z" #'focus-mode)

(setup dired
  (:bind "C-c t i" #'image-dired))

(setup dired-preview
  (:with-feature dired
    (:bind "C-c t p" #'dired-preview-global-mode)))

(setup org-modern
  (:with-feature org
    (:bind "C-c t p" #'org-modern-mode))
  (:with-feature org-agenda
    (:bind "C-c t p" #'org-modern-mode)))

;;;;; [C-c w] :: WORKSPACE

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


;;;;; [C-c z] :: FOLD

(define-keymap :keymap ceamx-fold-prefix
  "z" #'treesit-fold-toggle)

(setup treesit-fold
  (:bind "C-c z c" #'treesit-fold-close
         "C-c z C" #'treesit-fold-close-all
         "C-c z o" #'treesit-fold-open
         "C-c z O" #'treesit-fold-open-all
         "C-c z r" #'treesit-fold-open-recursively
         "C-c z t" #'treesit-fold-toggle))

;;;;; [C-x]

;;;;; [C-x w] :: Window Prefix

(define-keymap :keymap global-map
  "C-x w w" #'ace-window

  "C-x w SPC" #'transpose-frame

  "C-x w d" #'ace-delete-window
  "C-x w p" #'popper-toggle
  "C-x w P" #'popper-toggle-type
  "C-x w u" #'winner-undo
  "C-x w U" #'winner-redo

  "C-x w h" #'windmove-left
  "C-x w H" #'ceamx-window/move-left
  "C-x w j" #'windmove-down
  "C-x w J" #'ceamx-window/move-down
  "C-x w k" #'windmove-up
  "C-x w K" #'ceamx-window/move-up
  "C-x w l" #'windmove-right
  "C-x w L" #'ceamx-window/move-right

  "C-x w =" #'balance-windows
  "C-x w <" #'flip-frame
  "C-x w >" #'flop-frame
  "C-x w [" #'rotate-frame-clockwise
  "C-x w ]" #'rotate-frame-anticlockwise
  "C-x w {" #'rotate-frame
  "C-x w }" #'rotate-frame)


;;;;; [M-*] :: Global Meta-Modified

(define-keymap :keymap global-map
  "M-#" #'consult-register-load
  ;; "M-$" #'jinx-correct
  "M-*" #'tempel-insert
  "M-=" #'count-words
  "M-+" #'tempel-complete
  ;; "M-]" #'logos-forward-page-dwim
  ;; "M-[" #'logos-backward-page-dwim
  "M-'" #'consult-register-store
  "M-." #'embark-dwim

  ;; "M-DEL" #'ceamx/backward-kill-word
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
  ;; "M-k" #'ceamx-simple/kill-line-backward
  "M-K" #'consult-keep-lines
  "M-l" #'downcase-dwim
  "M-o" #'crux-other-window-or-switch-buffer
  ;; "M-o" #'ace-window
  "M-O" #'delete-blank-lines
  ;; "M-q" #'unfill-toggle
  "M-Q" #'repunctuate-sentences
  "M-u" #'upcase-dwim
  ;; "M-w" #'easy-kill
  "M-y" #'consult-yank-pop
  "M-z" #'zap-up-to-char
  ;; "M-Z" #'ceamx-simple/zap-to-char-backward
  )

;;;;; [M-g] :: Goto Map

(define-keymap :keymap (current-global-map)
  ;; "M-g d" #'ceamx/dogears-dispatch
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
  ;; "M-g M-b" #'dogears-back
  ;; "M-g M-f" #'dogears-forward
  ;; "M-g M-d" #'dogears-list
  ;; "M-g M-D" #'dogears-sidebar
  )

;;;;; [M-s] :: Search Prefix

(define-keymap :keymap global-map
  ;; "M-s b" #'ceamx-simple/buffers-major-mode
  "M-s c"  #'consult-locate
  "M-s d"  #'consult-fd                 ; or `consult-find'
  "M-s e"  #'consult-isearch-history
  "M-s g"  #'consult-ripgrep
  "M-s G"  #'consult-git-grep
  "M-s k"  #'consult-keep-lines
  "M-s l"  #'consult-line
  "M-s L"  #'consult-line-multi
  "M-s n" #'consult-notes
  "M-s r" '("[ REPLACE ]" . ceamx-replace-prefix)
  "M-s u"  #'consult-focus-lines
  ;; "M-s v" #'ceamx-simple/buffers-vc-root

  ;; "M-s M-f" #'org-node-find
  "M-s M-o" #'multi-occur
  "M-s M-s" #'consult-outline)

(setup substitute
  (define-keymap :keymap ceamx-replace-prefix
    "b" #'substitute-target-in-buffer
    "d" #'substitute-target-in-defun
    "s" #'substitute-target-above-point
    "S" #'substitute-target-below-point))

(setup consult
  (:with-feature isearch
    (:bind "M-s e" #'consult-isearch-history
	   "M-s l" #'consult-line
	   "M-s L" #'consult-line-multi)))


;;;; Finalize

(load-file (locate-user-emacs-file "custom.el"))

;;;;; Start the daemon

(setup emacs
  (:with-hook after-init-hook
    (:hook (defun ceamx-maybe-start-server-h ()
             (require 'server)
             (unless (server-running-p)
               (server-start))))))

;;;; Footer

(provide 'init)
;;; init.el ends here
