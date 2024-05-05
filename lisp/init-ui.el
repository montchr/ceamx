;;; init-ui.el --- General user interface customizations  -*- lexical-binding: t;  -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
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

;; Modal keybinding systems will change the cursor dynamically to indicate current state.
;; This default value matches what I expect in an "insert" mode.
(setq-default cursor-type 'bar)

;; Enable cursor blinking.
(blink-cursor-mode 1)

;; Seeing a cursor in a window other than the active window is pretty confusing.
(setq-default cursor-in-non-selected-windows nil)

;; Improve visual contrast between focused/non-focused windows.
(setopt highlight-nonselected-windows nil)
;;; Customization buffer and menu interface

(setopt custom-theme-allow-multiple-selections nil)

(setopt custom-unlispify-menu-entries nil)
(setopt custom-unlispify-tag-names nil)
(setopt custom-unlispify-remove-prefixes nil)
;;; Provide commonly-used interface libraries

;; Required as dependencies for many packages, either as more recent versions
;; than those available in Emacs (e.g. ~transient ~IIRC), or, including some
;; (like ~nix-mode~) who don't seem to declare them.

(package! transient)

(with-eval-after-load 'transient
  (defvar transient-map)
  (declare-function transient-quit-one "transient")

  ;; Always close transient with ESC
  ;; FIXME: meow overrides this. waiting until it loads does not help.
  (keymap-set transient-map "ESC" #'transient-quit-one))

(package! magit-section)

(package! nerd-icons
  (setopt nerd-icons-font-family "Symbols Nerd Font Mono")
  (require 'nerd-icons))

(package! svg-lib)

(package! hydra)
;;; ~pretty-hydra~

;; <https://github.com/jerrypnz/major-mode-hydra.el/#pretty-hydra>

(package! pretty-hydra)
;;; ~avy~

;; <https://github.com/abo-abo/avy>

;; <https://karthinks.com/software/avy-can-do-anything/>

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
;;; ~page-break-lines~: improve appearance of form feed characters

;; <https://github.com/purcell/page-break-lines/blob/master/README.md>

(package! page-break-lines
  (global-page-break-lines-mode))
;;; ~breadcrumb~: header-line indication of current location in project

;; <https://github.com/joaotavora/breadcrumb>

(package! breadcrumb
  (add-hook 'ceamx-after-init-hook #'breadcrumb-mode))
;;; ~pulsar~: Pulse current line after function invocations

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
;;; Requirements

(require 'config-env)
(require 'config-ui)

(require 'lib-ui)
;;; General Configuration

;; Don't prompt to confirm theme safety.
(setopt custom-safe-themes t)
;;; Ensure themes are applied in new frames to prevent flashing

;; TODO: also some other link i can't find now
;; <https://protesilaos.com/emacs/dotemacs#h:7d3a283e-1595-4692-8124-e0d683cb15b2>
(add-hook 'after-make-frame-functions #'ceamx-theme-re-enable-in-frame)
;;; Add a custom hook to run after enabling a theme

;; via <https://github.com/jdtsmith/kind-icon/issues/34#issuecomment-1668560185>

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
  ;; (setopt ceamx-theme-default-dark 'ef-night)
  ;; (setopt ceamx-theme-default-light 'ef-frost)

  ;; (setopt ef-themes-to-toggle (list ceamx-theme-default-dark ceamx-theme-default-light))

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

(require 'lib-common)
;;; Sunrise/sunset interval via ~solar~ and ~circadian~

(use-feature! solar
  :when (eq 'solar ceamx-theme-circadian-interval)

  :config
  (setopt calendar-latitude 39.968)
  (setopt calendar-longitude -75.133))

(use-package circadian
  :when (eq 'solar ceamx-theme-circadian-interval)
  :ensure t
  :demand t
  :after solar

  :commands (circadian-setup)

  :init
  (setopt circadian-themes `((:sunrise . ,ceamx-theme-default-light)
                             (:sunset . ,ceamx-theme-default-dark)))
  (circadian-setup))
;;;; Phase-of-day interval via `theme-buffet'

;; <https://git.sr.ht/~bboal/theme-buffet>

;; > The theme-buffet package arranges to automatically change themes during
;; > specific times of the day or at fixed intervals. The collection of themes
;; > is customisable, with the default options covering the built-in Emacs
;; > themes as well as Prot's modus-themes and ef-themes.

(use-package theme-buffet
  :ensure t
  :demand t
  :when (eq 'buffet ceamx-theme-circadian-interval)

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
;;;; Show current command and its binding with ~keycast~

;; <https://github.com/tarsius/keycast>

;; Supports display in the mode-line, header-line, tab-bar, and as messages in a
;; dedicated frame.

;; NOTE: Incompatible with kitchen-sink modeline packages like `doom-modeline'
;; and `telephone-line'.

(use-package keycast
  :commands (keycast-mode-line-mode)

  ;; :init
  ;; (add-hook 'ceamx-emacs-startup-hook #'keycast-mode-line-mode)

  :config
  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p mouse-movement-p mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil))))
(package! doom-modeline
  (setopt doom-modeline-support-imenu t)
  (setopt doom-modeline-unicode-fallback t)
  (setopt doom-modeline-buffer-encoding nil)
  (setopt doom-modeline-github nil)
  (setopt doom-modeline-buffer-file-name-style 'truncate-upto-project)

  ;; Enable HUD mode, providing a micromap of buffer position.
  (setopt doom-modeline-hud t)

  (setopt doom-modeline-icon t)

  ;; note that the major mode icon is not missing like most others.
  ;; git branch icon is also fine.
  (setopt doom-modeline-major-mode-icon t)

  ;; FIXME: missing icons when using nix-installed icon font
  (setopt doom-modeline-buffer-state-icon t)
  (setopt doom-modeline-buffer-modification-icon t)

  (setopt doom-modeline-modal t)
  ;; FIXME: missing icon with nix-installed font... but only when non-nil?! when nil, icon displays properly...
  (setopt doom-modeline-modal-icon t)

  (doom-modeline-mode 1))
(elpaca-wait)
(setopt ceamx-theme-default-light 'modus-operandi)
(setopt ceamx-theme-default-dark 'modus-vivendi-tinted)
(ceamx/load-dark-theme)
;;; ~spacious-padding~ :: <https://protesilaos.com/emacs/spacious-padding>
(use-package spacious-padding
  :demand t
  :commands (spacious-padding-mode)
  :defines (spacious-padding-widths)

  :init
  (setopt spacious-padding-widths
          '(
            ;; NOTE: `:internal-border-width' currently breaks `tab-bar-mode'
            ;;       display on Emacs 29. Fixed in master branch.
            ;;       <https://lists.gnu.org/r/bug-gnu-emacs/2023-07/msg01594.html>
            :internal-border-width 15
            :header-line-width 4
            :mode-line-width 4
            :tab-width 4
            :right-divider-width 30
            :scroll-bar-width 8))

  :config

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible.
  ;; TODO: v0.3.0 standardizes this a bit
  ;; (setq spacious-padding-subtle-mode-line
  ;;       `(:mode-line-active default     ; NOTE: assumes `modus-themes'
  ;;                           :mode-line-inactive vertical-border))

  (spacious-padding-mode 1))

(define-keymap :keymap ceamx-session-map
  "f" (cons "Font" (define-prefix-command 'ceamx-session-font-prefix-command))
  "f f" #'fontaine-set-preset
  "t" (cons "Theme" (define-prefix-command 'ceamx-session-theme-prefix-command))
  "t d" #'ceamx/dark
  "t l" #'ceamx/light
  "t o" #'olivetti-mode)

(provide 'init-ui)
;;; init-ui.el ends here
