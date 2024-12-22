;;; init-ui.el --- General user interface customizations  -*- lexical-binding: t;  -*-

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

(require 'ceamx-paths)
(require 'config-env)

(require 'ceamx-lib)
(require 'lib-ui)

;; Cursor


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

;; Customize the Customization buffer and menu interface


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

;; Consider all themes "safe"


(setopt custom-safe-themes t)

(add-hook 'after-make-frame-functions #'ceamx-ui-re-enable-theme-in-frame)

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

;; Set approximate stomping coordinates for hyper-astronomic relativity calculations


(require 'cal-dst)
(require 'config-ui)
(require 'ceamx-lib)

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

;; ~rainbow-mode~: Colorize color names and hexcodes in buffers

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

;; Allow restoring deleted frames :history:


(undelete-frame-mode 1)

;; Configure frame decorations :graphical:


(unless +sys-mac-p
  ;; Hide window decorations.
  (add-to-list 'default-frame-alist '(undecorated . t)))

;; Handle macOS-specific workarounds :macos:


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

;; Add frame borders and window dividers :window:


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

;; Configure tab bar appearance and behavior


(setopt tab-bar-auto-width t
        tab-bar-auto-width-max '((80) 10))

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

;; Keybindings :keybinds:


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
