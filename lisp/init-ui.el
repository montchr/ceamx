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


;; [[file:../config.org::*Cursor][Cursor:1]]
;; Modal keybinding systems will change the cursor dynamically to indicate current state.
;; This value matches what I expect in an "insert" mode.
(setq-default cursor-type 'bar)

;; Enable cursor blinking.
(blink-cursor-mode 1)

;; Seeing a cursor in a window other than the active window is pretty confusing.
(setq-default cursor-in-non-selected-windows nil)
;; Cursor:1 ends here

;; Customize the Customization buffer and menu interface


;; [[file:../config.org::*Customize the Customization buffer and menu interface][Customize the Customization buffer and menu interface:1]]
(setopt custom-theme-allow-multiple-selections nil)

(setopt custom-unlispify-menu-entries nil)
(setopt custom-unlispify-tag-names nil)
(setopt custom-unlispify-remove-prefixes nil)

(add-hook 'Custom-mode-hook #'custom-toggle-hide-all-widgets nil t)
;; Customize the Customization buffer and menu interface:1 ends here

;; =grid=: Library for textual data table presentation :package:

;; - Source :: [[https://github.com/ichernyshovvv/grid.el][ichernyshovvv/grid.el]]
;; - Retrieved :: [2024-06-07 Fri 11:45]

;; #+begin_quote
;; This library allows you to put text data into boxes and align them horizontally,
;; applying margin, padding, borders.
;; #+end_quote


;; [[file:../config.org::*=grid=: Library for textual data table presentation][=grid=: Library for textual data table presentation:1]]
(package! (grid :host github :repo "ichernyshovvv/grid.el"))
;; =grid=: Library for textual data table presentation:1 ends here

;; =hydra= :package:hydra:

;; - Documentation :: <https://github.com/jerrypnz/major-mode-hydra.el/#pretty-hydra>


;; [[file:../config.org::*=hydra=][=hydra=:1]]
(package! hydra)
(package! pretty-hydra)
;; =hydra=:1 ends here

;; =transient= :package:transient:


;; [[file:../config.org::*=transient=][=transient=:1]]
(package! transient
  ;; Restore the default location, overriding `no-littering'.  I consider these
  ;; values configuration to be exposed, not state to be hidden.  See
  ;; `transient-save-values' and related.
  (setopt transient-values-file (locate-user-emacs-file "transient/values.el")))

(package! magit-section)
;; =transient=:1 ends here

;; Close any ~transient~ menu with the escape key


;; [[file:../config.org::*Close any ~transient~ menu with the escape key][Close any ~transient~ menu with the escape key:1]]
(with-eval-after-load 'transient
  (keymap-set transient-map "<escape>" #'transient-quit-one))
;; Close any ~transient~ menu with the escape key:1 ends here

;; Consider all themes "safe"


;; [[file:../config.org::*Consider all themes "safe"][Consider all themes "safe":1]]
(setopt custom-safe-themes t)
;; Consider all themes "safe":1 ends here

;; [[file:../config.org::*Ensure themes are applied in new frames to prevent flashing][Ensure themes are applied in new frames to prevent flashing:2]]
(add-hook 'after-make-frame-functions #'ceamx-ui-re-enable-theme-in-frame)
;; Ensure themes are applied in new frames to prevent flashing:2 ends here

;; Add a custom hook ~ceamx-after-enable-theme-hook~ to run after enabling a theme

;; - Source :: <https://github.com/jdtsmith/kind-icon/issues/34#issuecomment-1668560185>


;; [[file:../config.org::*Add a custom hook ~ceamx-after-enable-theme-hook~ to run after enabling a theme][Add a custom hook ~ceamx-after-enable-theme-hook~ to run after enabling a theme:1]]
(defvar ceamx-after-enable-theme-hook nil)

(defun ceamx-after-enable-theme (&rest _args)
  "Hook to run after enabling theme."
  (run-hooks 'ceamx-after-enable-theme-hook))

(advice-add 'enable-theme :after #'ceamx-after-enable-theme)
;; Add a custom hook ~ceamx-after-enable-theme-hook~ to run after enabling a theme:1 ends here

;; Modus Themes :package:

;; - Website :: <https://protesilaos.com/modus-themes/>


;; [[file:../config.org::*Modus Themes][Modus Themes:1]]
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
;; Modus Themes:1 ends here

;; Ef-Themes :package:

;; - Website :: <https://protesilaos.com/emacs/ef-themes>


;; [[file:../config.org::*Ef-Themes][Ef-Themes:1]]
(defvar ceamx-font-headings-style-alist)
;; Ef-Themes:1 ends here

;; [[file:../config.org::*Ef-Themes][Ef-Themes:2]]
(package! ef-themes
  (require 'ef-themes)

  (setopt ef-themes-to-toggle '(ef-night ef-frost)
          ef-themes-mixed-fonts t
          ef-themes-variable-pitch-ui nil))
;; Ef-Themes:2 ends here

;; Set approximate stomping coordinates for hyper-astronomic relativity calculations


;; [[file:../config.org::*Set approximate stomping coordinates for hyper-astronomic relativity calculations][Set approximate stomping coordinates for hyper-astronomic relativity calculations:1]]
(require 'cal-dst)
(require 'config-ui)
(require 'ceamx-lib)
;; Set approximate stomping coordinates for hyper-astronomic relativity calculations:1 ends here

;; [[file:../config.org::*Set approximate stomping coordinates for hyper-astronomic relativity calculations][Set approximate stomping coordinates for hyper-astronomic relativity calculations:2]]
(setopt calendar-latitude 39.968)
(setopt calendar-longitude -75.133)
;; Set approximate stomping coordinates for hyper-astronomic relativity calculations:2 ends here

;; =circadian=: theme phasing based on sunrise/sunset time :package:


;; [[file:../config.org::*=circadian=: theme phasing based on sunrise/sunset time][=circadian=: theme phasing based on sunrise/sunset time:1]]
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
;; =circadian=: theme phasing based on sunrise/sunset time:1 ends here

;; Elpaca-Wait № 4: ensure availability of themes for integration :wait:


;; [[file:../config.org::*Elpaca-Wait № 4: ensure availability of themes for integration][Elpaca-Wait № 4: ensure availability of themes for integration:1]]
(elpaca-wait)
;; Elpaca-Wait № 4: ensure availability of themes for integration:1 ends here

;; Load a default theme

;; Configure some user options dependent on the loaded packages:


;; [[file:../config.org::*Load a default theme][Load a default theme:1]]
(setopt ceamx-ui-theme-light 'modus-operandi-tinted)
(setopt ceamx-ui-theme-dark 'modus-vivendi)
;; Load a default theme:1 ends here

;; [[file:../config.org::*Load a default theme][Load a default theme:2]]
(if (eq 'solar ceamx-ui-theme-circadian-interval)
    (after! circadian (add-hook 'ceamx-after-init-hook #'circadian-setup))
  (if (ceamx-ui-desktop-dark-theme-p)
      (ceamx-ui/load-dark-theme)
    (ceamx-ui/load-light-theme)))
;; Load a default theme:2 ends here

;; Avy :package:

;; - Website :: <https://github.com/abo-abo/avy>
;; - Ref :: <https://karthinks.com/software/avy-can-do-anything/>


;; [[file:../config.org::*Avy][Avy:1]]
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
;; Avy:1 ends here

;; ~rainbow-mode~: Colorize color names and hexcodes in buffers

;; <https://elpa.gnu.org/packages/rainbow-mode.html>


;; [[file:../config.org::*~rainbow-mode~: Colorize color names and hexcodes in buffers][~rainbow-mode~: Colorize color names and hexcodes in buffers:1]]
(package! rainbow-mode)
;; ~rainbow-mode~: Colorize color names and hexcodes in buffers:1 ends here

;; Highlight the current line with ~hl-line-mode~ [builtin]


;; [[file:../config.org::*Highlight the current line with ~hl-line-mode~ \[builtin\]][Highlight the current line with ~hl-line-mode~ [builtin]:1]]
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'package-menu-mode-hook #'hl-line-mode)
;; Highlight the current line with ~hl-line-mode~ [builtin]:1 ends here

;; Differentiate between focused and non-focused windows :window:


;; [[file:../config.org::*Differentiate between focused and non-focused windows][Differentiate between focused and non-focused windows:1]]
(setopt highlight-nonselected-windows nil)
;; Differentiate between focused and non-focused windows:1 ends here

;; =pulsar=: pulse current line after function invocations :package:animation:


;; [[file:../config.org::*=pulsar=: pulse current line after function invocations][=pulsar=: pulse current line after function invocations:1]]
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
;; =pulsar=: pulse current line after function invocations:1 ends here

;; Allow restoring deleted frames :history:


;; [[file:../config.org::*Allow restoring deleted frames][Allow restoring deleted frames:1]]
(undelete-frame-mode 1)
;; Allow restoring deleted frames:1 ends here

;; Configure frame decorations :graphical:


;; [[file:../config.org::*Configure frame decorations][Configure frame decorations:1]]
(unless +sys-mac-p
  ;; Hide window decorations.
  (add-to-list 'default-frame-alist '(undecorated . t)))
;; Configure frame decorations:1 ends here

;; Handle macOS-specific workarounds :macos:


;; [[file:../config.org::*Handle macOS-specific workarounds][Handle macOS-specific workarounds:1]]
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
;; Handle macOS-specific workarounds:1 ends here

;; Add frame borders and window dividers :window:


;; [[file:../config.org::*Add frame borders and window dividers][Add frame borders and window dividers:1]]
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
;; Add frame borders and window dividers:1 ends here

;; Menu Bar :menubar:

;; Disable the menu bar by default:


;; [[file:../config.org::*Menu Bar][Menu Bar:1]]
(menu-bar-mode -1)
;; Menu Bar:1 ends here



;; But allow toggling it manually:


;; [[file:../config.org::*Menu Bar][Menu Bar:2]]
(keymap-set ceamx-toggle-map "M" #'menu-bar-mode)
;; Menu Bar:2 ends here

;; Enable ~tab-bar-mode~ in Emacs 30

;; - ref :: <https://lists.gnu.org/r/bug-gnu-emacs/2023-07/msg01594.html>

;; ~tab-bar-mode~ is currently broken in Emacs 29 due to upstream bug.  The fix is
;; present on the =master= branch (Emacs 30), but it will not be backported.

;; Unfortunately, the bug is impossibly distracting.  So I am avoiding
;; `tab-bar-mode' on Emacs 29.

;; As of <2024-06-06>, I am using the =nix-community/emacs-overlay#emacs-pgtk= package tracking the
;; Emacs =master= branch.  ~tab-bar-mode~ is that important to me.  Emacs 30 seems
;; stable enough so far.


;; [[file:../config.org::*Enable ~tab-bar-mode~ in Emacs 30][Enable ~tab-bar-mode~ in Emacs 30:1]]
(unless (version< emacs-version "30")
  (tab-bar-mode 1))
;; Enable ~tab-bar-mode~ in Emacs 30:1 ends here

;; Configure tab bar appearance and behavior


;; [[file:../config.org::*Configure tab bar appearance and behavior][Configure tab bar appearance and behavior:1]]
(setopt tab-bar-auto-width t
        tab-bar-auto-width-max '((80) 10))
;; Configure tab bar appearance and behavior:1 ends here

;; Provide common dependency: ~nerd-icons~ :package:


;; [[file:../config.org::*Provide common dependency: ~nerd-icons~][Provide common dependency: ~nerd-icons~:1]]
(package! nerd-icons
  (setopt nerd-icons-font-family "Symbols Nerd Font Mono")
  (require 'nerd-icons))
;; Provide common dependency: ~nerd-icons~:1 ends here

;; Provide common dependency: ~svg-lib~ :package:


;; [[file:../config.org::*Provide common dependency: ~svg-lib~][Provide common dependency: ~svg-lib~:1]]
(package! svg-lib)
;; Provide common dependency: ~svg-lib~:1 ends here

;; ~page-break-lines~: Improve appearance of form feed characters :package:

;; - docs :: <https://github.com/purcell/page-break-lines/blob/master/README.md>


;; [[file:../config.org::*~page-break-lines~: Improve appearance of form feed characters][~page-break-lines~: Improve appearance of form feed characters:1]]
(package! page-break-lines
  (global-page-break-lines-mode))
;; ~page-break-lines~: Improve appearance of form feed characters:1 ends here

;; [[file:../config.org::*Modeline][Modeline:2]]
(line-number-mode 1)
(column-number-mode 1)

(setopt display-time-24hr-format t)
;; Modeline:2 ends here

;; Show current command and its binding with ~keycast~

;; - Website :: <https://github.com/tarsius/keycast>

;; Supports display in the mode-line, header-line, tab-bar, and as messages in a
;; dedicated frame.

;; NOTE: Incompatible with kitchen-sink modeline packages like =doom-modeline= and
;; =telephone-line=.


;; [[file:../config.org::*Show current command and its binding with ~keycast~][Show current command and its binding with ~keycast~:1]]
(package! keycast
  (keymap-set ceamx-toggle-map "k" #'keycast-mode-line-mode))
;; Show current command and its binding with ~keycast~:1 ends here

;; [[file:../config.org::*Show current command and its binding with ~keycast~][Show current command and its binding with ~keycast~:2]]
(after! keycast
  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p mouse-movement-p mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil))))
;; Show current command and its binding with ~keycast~:2 ends here

;; Keybindings :keybinds:


;; [[file:../config.org::*Keybindings][Keybindings:1]]
(define-keymap :keymap ceamx-session-map
  "a" (cons "Appearance" (define-prefix-command 'ceamx-session-appearance-prefix-command))
  "a f" #'fontaine-set-preset
  "a d" #'ceamx-ui/dark
  "a l" #'ceamx-ui/light
  "a o" #'olivetti-mode

  "f" (cons "Frame" (define-prefix-command 'ceamx-session-f-prefix))
  "f d" #'delete-frame)
;; Keybindings:1 ends here

(provide 'init-ui)
;;; init-ui.el ends here
