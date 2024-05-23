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
(package! hydra)
(package! pretty-hydra)
(package! transient)
(package! magit-section)
(with-eval-after-load 'transient
  (defvar transient-map)
  (declare-function transient-quit-one "transient")

  ;; Always close transient with ESC
  ;; FIXME: meow overrides this. waiting until it loads does not help.
  (keymap-set transient-map "ESC" #'transient-quit-one))
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
  (setopt modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))

  ;; FIXME: results in type warning, but works
  (setopt modus-themes-headings
          '((1 . (variable-pitch regular background 1.4))
            (2 . (variable-pitch regular background 1.2))
            (3 . (background 1.1))
            (4 . (1.1))
            (t . t)))

  (setopt modus-themes-common-palette-overrides
          '(
            ;; Make the fringe invisible.
            (fringe unspecified)
            ;; Make line numbers less intense and add a shade of cyan
            ;; for the current line number.
            (fg-line-number-inactive "gray50")
            (fg-line-number-active cyan-cooler)
            (bg-line-number-inactive unspecified)
            (bg-line-number-active unspecifed)))

  (def-hook! +modus-themes-custom-faces-h ()
    'modus-themes-after-load-theme-hook
    "Configurate custom faces for the `modus-themes'."
    (modus-themes-with-colors
      (custom-set-faces
       ;; Add "padding" to the mode lines.
       `(mode-line ((,c :box (:line-width 10
                              :color ,bg-mode-line-active))))
       `(mode-line-inactive ((,c :box (:line-width 10
                                       :color ,bg-mode-line-inactive)))))))

  ;; Do not extend `region' background past the end of the line.
  ;; <https://protesilaos.com/emacs/modus-themes#h:a5140c9c-18b2-45db-8021-38d0b5074116>
  (custom-set-faces
   '(region ((t :extend nil)))))
(defvar ceamx-font-headings-style-alist)
(use-package ef-themes
  :ensure t
  :demand t
  :commands (ef-themes-select)

  :config
  ;; (setopt ceamx-ui-theme-dark 'ef-night)
  ;; (setopt ceamx-ui-theme-light 'ef-frost)

  ;; (setopt ef-themes-to-toggle (list ceamx-ui-theme-dark ceamx-ui-theme-light))

  (setopt ef-themes-headings
          '((0 . (variable-pitch 1.4))
            (1 . (variable-pitch 1.3))
            (2 . (variable-pitch semibold 1.25))
            (3 . (variable-pitch medium 1.2))
            (4 . (variable-pitch regular 1.15))
            (5 . (variable-pitch semilight 1.05))
            (agenda-date . (semilight 1.5))
            (agenda-structure . (variable-pitch light 1.4))
            (t . (variable-pitch 0.9))))

  (setopt ef-themes-mixed-fonts t
          ef-themes-variable-pitch-ui nil)

  ;; Disable all other themes to avoid awkward blending
  (mapc #'disable-theme custom-enabled-themes))
(require 'cal-dst)

(require 'config-ui)

(require 'ceamx-lib)
(use-feature! solar
  :when (eq 'solar ceamx-ui-theme-circadian-interval)

  :config
  (setopt calendar-latitude 39.968)
  (setopt calendar-longitude -75.133))

(use-package circadian
  :when (eq 'solar ceamx-ui-theme-circadian-interval)
  :ensure t
  :demand t
  :after solar

  :commands (circadian-setup)

  :init
  (setopt circadian-themes `((:sunrise . ,ceamx-ui-theme-light)
                             (:sunset . ,ceamx-ui-theme-dark)))
  (circadian-setup))
(use-package theme-buffet
  :ensure t
  :demand t
  :when (eq 'buffet ceamx-ui-theme-circadian-interval)

  :commands (theme-buffet-modus-ef)
  :defines (theme-buffet-menu)

  :init

  ;; Take Daylight Savings Time offset into account for time period boundaries.
  ;; I am not sure why the additional `1+' is necessary, but this is copied from
  ;; the author's recommendation.
  ;; via <https://git.sr.ht/~bboal/theme-buffet/tree/06f1be349e9c3d124520b18742911307de9abda3/item/theme-buffet.el#L68-70>
  (setopt theme-buffet-time-offset (1+ (/ (cadr (calendar-current-time-zone)) 60)))

  (setopt theme-buffet-menu 'end-user)

  (setopt theme-buffet-end-user
          '(:night (;; ef-autumn
                    ef-duo-dark
                    ef-trio-dark
                    ef-night
                    ef-winter
                    ef-dark)
            :twilight (ef-bio
                       ef-cherie
                       ef-dream
                       ef-rosa
                       modus-vivendi)
            :morning (ef-elea-light
                      ef-maris-light
                      ef-reverie
                      ef-spring)
            :day (ef-frost
                  ef-light
                  ef-spring
                  ef-trio-light
                  modus-operandi)
            :afternoon (ef-cyprus
                        ;; ef-arbutus
                        ef-day
                        ef-duo-light
                        ef-kassio
                        ef-melissa-light
                        ef-reverie
                        ef-summer
                        modus-operandi-tinted)
            :evening (ef-elea-dark
                      ef-dream
                      ef-maris-dark
                      ;; ef-melissa-dark
                      ef-symbiosis
                      ef-trio-dark
                      modus-vivendi-tinted)))

  :config

  (theme-buffet-end-user)

  ;; Activate some theme in the current period.
  (theme-buffet-a-la-carte))
(defun ceamx-ui-desktop-dark-theme-p ()
  "Predicate whether a desktop environment is displaying a dark appearance."
  (or (ceamx-ui-gsettings-dark-theme-p)))
(elpaca-wait)
(setopt ceamx-ui-theme-light 'modus-operandi)
(setopt ceamx-ui-theme-dark 'modus-vivendi-tinted)
(if (ceamx-ui-desktop-dark-theme-p)
    (ceamx/ui/load-dark-theme)
  (ceamx/ui/load-light-theme))
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
  (setopt pulsar-pulse t
          pulsar-delay 0.055
          pulsar-iterations 10
          pulsar-face 'pulsar-magenta
          pulsar-highlight-face 'pulsar-cyan)

  (pulsar-global-mode 1)

  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line)

  (dolist (fn '(pulsar-pulse-line-red pulsar-recenter-top pulsar-reveal-entry))
    (add-hook 'next-error-hook (function fn))))
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
;; Disable the frame menu bar by default
(menu-bar-mode -1)

;; Enable the resurrection of frames with ~undelete-frame~
(undelete-frame-mode 1)
(modify-all-frames-parameters
 ;; NOTE: `org-modern', whose readme provided this example, initially had these
 ;; set to 40.  but on <%tuvok%>, 40 looked absolutely ridiclous.  i figured i
 ;; should try halving that, which seemed perfect.  so i am guessing there is
 ;; some confodungin factors with high-density displays
 '((right-divider-width . 20)
   (internal-border-width . 20)))

(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))

(set-face-background 'fringe (face-attribute 'default :background))
(setopt highlight-nonselected-windows nil)
(package! nerd-icons
  (setopt nerd-icons-font-family "Symbols Nerd Font Mono")
  (require 'nerd-icons))
(package! svg-lib)
(package! page-break-lines
  (global-page-break-lines-mode))
(use-feature! emacs
  :config
  (setopt line-number-mode t)
  (setopt column-number-mode t))

(use-feature! time
  :config
  (setopt display-time-24hr-format t))
(use-package minions
  :commands (minions-mode)
  :config
  (minions-mode 1))
(use-package keycast
  :commands (keycast-mode-line-mode)

  ;; :init
  ;; (add-hook 'ceamx-emacs-startup-hook #'keycast-mode-line-mode)

  :config
  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p mouse-movement-p mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil))))
(define-keymap :keymap ceamx-session-map
  "a" (cons "Appearance" (define-prefix-command 'ceamx-session-font-prefix-command))
  "a f" #'fontaine-set-preset
  "a d" #'ceamx/dark
  "a l" #'ceamx/light
  "a o" #'olivetti-mode

  "f" (cons "Frame" (define-prefix-command 'ceamx-session-f-prefix))
  "f d" #'delete-frame)

(provide 'init-ui)
;;; init-ui.el ends here
