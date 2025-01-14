;;; early-init.el --- Early initialization file  -*- lexical-binding: t; no-byte-compile: t; -*-

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

;; Prevent package.el from enabling all packages before init

;; - [[https://old.reddit.com/r/emacs/comments/np6ey4/how_packageel_works_with_use_package/][How package.el Works with Use Package : emacs]]


;; When nil and using the builtin package manager, ~package-initialize~ must be
;; invoked in the init process prior to ~require~ing any packages installed with
;; ~package-install~.

;; When non-nil, there is no need to invoke ~package-initialize~.


(setq package-enable-at-startup nil)

;; Define proxy hooks for ~after-init-hook~ and ~emacs-startup-hook~


(defvar ceamx-after-init-hook '())
(defun ceamx-after-init-hook ()
  (run-hooks 'ceamx-after-init-hook))

(defvar ceamx-emacs-startup-hook '())
(defun ceamx-emacs-startup-hook ()
  (run-hooks 'ceamx-emacs-startup-hook))

;; Provide insight into garbage-collection activity to inform tuning decisions


;; TODO: will a `init-file-debug' check work here?
(setq garbage-collection-messages t)

;; Prevent garbage-collection during init


;; NOTE: Either use `gcmh' or make sure to reset this later.  Or else!
(setq gc-cons-threshold (* 128 1024 1024)) ; 128MiB

;; Add directories to load path


;; Configure load path
(dolist (subdir '("autoloads" "lisp" "lisp/core" "lisp/lib"))
  (let ((dir (expand-file-name subdir user-emacs-directory)))
    (add-to-list 'load-path dir)))

;; Load custom constants describing well-known paths

;; See [[*=ceamx-paths= :: common path constants]]


(require 'ceamx-paths)

;; Store packages in the designated directory


(setq package-user-dir ceamx-packages-dir)

;; Use preferred cache directories for native compilation


(startup-redirect-eln-cache ceamx-eln-dir)
(add-to-list 'native-comp-eln-load-path ceamx-eln-dir)

;; Native compilation settings


(setq native-comp-async-report-warnings-errors 'silent)
(setq native-compile-prune-cache t)

;; Don't load outdated byte-compiled files.
;;
;; NOTE: This does not handle *recompiling* the outdated files.
;; That would need to be handled during init.
;;
;; More info: <https://github.com/emacscollective/auto-compile/blob/main/README.org>
(setq load-prefer-newer t)

;; Package installation will provoke a lot of warnings from third-party
;; packages, but there's nothing we can do about those.
(setq byte-compile-warnings nil)

;; Disable the bell


(setq ring-bell-function #'ignore)

;; Appearance: integrate with desktop environment


;; Prevent X11 from taking control of visual behavior and appearance.
(setq inhibit-x-resources t)

;; FIXME these should be renamed or moved as their namespace indicates
;; they would normally live in the ~ceamx-ui~ feature.


(defconst ceamx-ui-gsettings-ui-namespace "org.gnome.desktop.interface")

(defun ceamx-ui-gsettings-theme ()
  "Get the currently-active GNOME/GTK color scheme."
  (shell-command-to-string (format "gsettings get %s color-scheme"
                         ceamx-ui-gsettings-ui-namespace)))

(defun ceamx-ui-gsettings-dark-theme-p ()
  "Whether GNOME/GTK are using a theme with a dark color scheme."
  (string-match-p "dark" (ceamx-ui-gsettings-theme)))

(defun ceamx-ui-desktop-dark-theme-p ()
  "Predicate whether a desktop environment is displaying a dark appearance."
  (or (ceamx-ui-gsettings-dark-theme-p)))

(defun ceamx-ui-re-enable-theme-in-frame (_frame)
  "Re-enable active theme, if any, upon FRAME creation.
Add this to `after-make-frame-functions' so that new frames do
not retain the generic background set by the function
`ceamx-prevent-initial-light-flash'."
  (when-let* ((theme (car custom-enabled-themes)))
    (enable-theme theme)))

;; Appearance: basic frame settings


(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b")
      use-dialog-box t
      use-file-dialog nil
      use-short-answers t              ; affects `yes-or-no-p'
      read-answer-short t              ; affects `read-answer' (completion)
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; `tooltip-mode' is broken for me in pgtk -- might be an Emacs bug,
;; causes constant errors when moving mouse over modeline.
;;
;; FIXME: actually, this is behaving inconsistently: disabling it does
;; not necessarily work, and toggling it off/on allows `tooltip-mode'
;; to function normally...  maybe needs to happen later in init?
(tooltip-mode -1)

;; Appearance: avoid flash of light in a dark environment

;; - source :: <https://protesilaos.com/emacs/dotemacs#h:7d3a283e-1595-4692-8124-e0d683cb15b2>



(defun ceamx-init-prevent-initial-light-flash ()
  "Avoid the bright flash of light during startup in dark environments."
  (when (ceamx-ui-desktop-dark-theme-p)
    (set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")
    (set-face-attribute 'mode-line nil :background "#000000" :foreground "#ffffff" :box 'unspecified)
    ;; FIXME: errors wrong num args
    ;;    (add-hook 'ceamx-after-init-hook #'ceamx-ui-re-enable-theme-in-frame)
    ;; (add-hook 'after-make-frame-functions #'ceamx-ui-re-enable-theme-in-frame)
    ))

;; (ceamx-init-prevent-initial-light-flash)

;; Rename the default/initial frame


(defvar ceamx-default-frame-name "home"
  "Name for the default Emacs frame.")

(defun ceamx-after-init-default-frame-name-h ()
  "Set the name for the default frame.
Simple wrapper for a call to `set-frame-name' providing
`ceamx-default-frame-name' as the NAME argument.

Intended for use as a callback on the `ceamx-after-init-hook'."
  (set-frame-name ceamx-default-frame-name))

(add-hook 'ceamx-after-init-hook #'ceamx-after-init-default-frame-name-h)
