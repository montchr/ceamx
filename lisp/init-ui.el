;;; init-ui.el --- General user interface customizations  -*- lexical-binding: t;  -*-

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

(require 'ceamx-paths)
(require 'config-env)

(require 'ceamx-lib)
(require 'lib-ui)
;; Modal keybinding systems will change the cursor dynamically to indicate current state.
;; This value matches what I expect in an "insert" mode.
(setq-default cursor-type 'bar)

;; Enable cursor blinking.
(blink-cursor-mode 1)

;; Seeing a cursor in a window other than the active window is pretty confusing.
(setq-default cursor-in-non-selected-windows nil)
(setopt custom-theme-allow-multiple-selections nil)

(setopt custom-unlispify-menu-entries nil)
(setopt custom-unlispify-tag-names nil)
(setopt custom-unlispify-remove-prefixes nil)

(add-hook 'Custom-mode-hook #'custom-toggle-hide-all-widgets nil t)
(package! (grid :host github :repo "ichernyshovvv/grid.el"))
(package! hydra)
(package! pretty-hydra)
(package! transient
  ;; Restore the default location, overriding `no-littering'.  I consider these
  ;; values configuration to be exposed, not state to be hidden.  See
  ;; `transient-save-values' and related.
  (setopt transient-values-file (locate-user-emacs-file "transient/values.el")))

(package! magit-section)
(with-eval-after-load 'transient
  (keymap-set transient-map "<escape>" #'transient-quit-one))
(setopt custom-safe-themes t)
(add-hook 'after-make-frame-functions #'ceamx-ui-re-enable-theme-in-frame)
(defvar ceamx-after-enable-theme-hook nil)

(defun ceamx-after-enable-theme (&rest _args)
  "Hook to run after enabling theme."
  (run-hooks 'ceamx-after-enable-theme-hook))

(advice-add 'enable-theme :after #'ceamx-after-enable-theme)
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
(defvar ceamx-font-headings-style-alist)
(package! ef-themes
  (require 'ef-themes)

  (setopt ef-themes-to-toggle '(ef-night ef-frost)
          ef-themes-mixed-fonts t
          ef-themes-variable-pitch-ui nil))
(setopt ceamx-ui-theme-circadian-interval 'solar)
(require 'cal-dst)
(require 'config-ui)
(require 'ceamx-lib)

(setopt calendar-latitude 39.968)
(setopt calendar-longitude -75.133)

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
;; FIXME: error open /dev/tty: no such address or device
(defun ceamx-ui-kitty-set-theme (polarity)
  "Set the Kitty terminal emulator colors to POLARITY.
POLARITY is a string matching either \"light\" or \"dark\"."
  (shell-command
   (format "kitty @set-colors -a -c $KITTY_CONFIG_DIRECTORY/theme-%s.conf"
           polarity)))
(defun ceamx-ui-desktop-dark-theme-p ()
  "Predicate whether a desktop environment is displaying a dark appearance."
  (or (ceamx-ui-gsettings-dark-theme-p)))
(elpaca-wait)
(setopt ceamx-ui-theme-light 'modus-operandi-tinted)
(setopt ceamx-ui-theme-dark 'modus-vivendi)
(if (eq 'solar ceamx-ui-theme-circadian-interval)
    (after! circadian (add-hook 'ceamx-after-init-hook #'circadian-setup))
  (if (ceamx-ui-desktop-dark-theme-p)
      (ceamx-ui/load-dark-theme)
    (ceamx-ui/load-light-theme)))
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
(undelete-frame-mode 1)
(unless +sys-mac-p
  ;; Hide window decorations.
  (add-to-list 'default-frame-alist '(undecorated . t)))
(when +sys-mac-p
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
(modify-all-frames-parameters
 '((right-divider-width . 20)
   (internal-border-width . 20)))

(defun ceamx-ui-theme-update-colors-h ()
"Set faces based on theme colors."
  (let ((bg-main (face-background 'default))
        (fg-main (face-foreground 'default)))
    (custom-set-faces
     `(fringe ((t :background ,bg-main)))
     `(tab-bar-tab
       ((t :box (:line-width 2
                 :color ,(face-background 'tab-bar-tab nil 'tab-bar)))))
     `(tab-bar-tab-inactive
       ((t :box (:line-width 2
                 :color ,(face-background 'tab-bar-tab-inactive nil 'tab-bar)))))
     ;; `(tab-line-tab
     ;;   ((t :box (:line-width 1
     ;;             :color ,(face-background 'tab-line-tab nil 'tab-bar)))))
     ;; `(tab-line-tab-active
     ;;   ((t :box (:line-width 1
     ;;             :color ,(face-background 'tab-line-tab-active nil 'tab-bar)))))
     ;; `(tab-line-tab-inactive
     ;;   ((t :box (:line-width 1
     ;;             :color ,(face-background 'tab-line-tab-inactive nil 'tab-bar)))))
     `(window-divider ((t :background ,bg-main :foreground ,bg-main)))
     `(window-divider-first-pixel ((t :background ,bg-main :foreground ,bg-main)))
     `(window-divider-last-pixel ((t :background ,bg-main :foreground ,bg-main))))))

(remove-hook 'ceamx-after-enable-theme-hook #'ceamx-ui-theme-update-colors-h)
(add-hook 'ceamx-after-enable-theme-hook #'ceamx-ui-theme-update-colors-h)
(setopt highlight-nonselected-windows nil)
(menu-bar-mode -1)
(keymap-set ceamx-toggle-map "M" #'menu-bar-mode)
(unless (version< emacs-version "30")
  (tab-bar-mode 1))
(setopt tab-bar-auto-width t
        tab-bar-auto-width-max '((80) 10))
(package! nerd-icons
  (setopt nerd-icons-font-family "Symbols Nerd Font Mono")
  (require 'nerd-icons))
(package! svg-lib)
(package! page-break-lines
  (global-page-break-lines-mode))
(line-number-mode 1)
(column-number-mode 1)

(setopt display-time-24hr-format t)
(package! keycast
  (keymap-set ceamx-toggle-map "k" #'keycast-mode-line-mode))
(after! keycast
  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p mouse-movement-p mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil))))
(define-keymap :keymap ceamx-session-map
  "a" (cons "Appearance" (define-prefix-command 'ceamx-session-appearance-prefix-command))
  "a f" #'fontaine-set-preset
  "a d" #'ceamx-ui/dark
  "a l" #'ceamx-ui/light
  "a o" #'olivetti-mode

  "f" (cons "Frame" (define-prefix-command 'ceamx-session-f-prefix))
  "f d" #'delete-frame)

(provide 'init-ui)
;;; init-ui.el ends here
