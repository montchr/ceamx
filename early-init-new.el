;;; early-init.el --- Early Init File  -*- no-byte-compile: t; -*-

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

(setq package-enable-at-startup nil)

(defvar ceamx-after-init-hook '())
(defun ceamx-after-init-hook ()
  (run-hooks 'ceamx-after-init-hook))

(defvar ceamx-emacs-startup-hook '())
(defun ceamx-emacs-startup-hook ()
  (run-hooks 'ceamx-emacs-startup-hook))

;; <https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process>

(setenv "LSP_USE_PLISTS" "true")

;; Read JSON streams in 1MiB chunks instead of the default 4kB.
;;
;; Language server responses tend to be in the 800kB to 3MB range,
;; according to the lsp-mode documentation (linked above).
;;
;; This is a general LSP concern, not specific to any particular implementation.
(when (functionp 'json-serialize)
  (setq read-process-output-max (* 1024 1024)))

;; Provide insight into garbage-collection activity to inform tuning decisions.
;; TODO: will a `init-file-debug' check work here?
(setq garbage-collection-messages t)

;; Prevent garbage-collection during init.
;; NOTE: Either use `gcmh' or make sure to reset this later. Or else!
(setq gc-cons-threshold (* 128 1024 1024)) ; 128MiB

;;  <https://github.com/jwiegley/dot-emacs/blob/79bc2cff3a28ecd1a315609bbb607eb4ba700f76/init.org#during-loading-of-this-module-clear-file-name-handler-alist>
;;  <https://old.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/>

(defvar ceamx-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun ceamx-restore-file-name-handler-alist-h ()
  "Restore the original value of the `file-name-handler-alist' variable.
Intended for use as a callback on `ceamx-after-init-hook'."
  (setq file-name-handler-alist ceamx-file-name-handler-alist)
  (makunbound 'ceamx-file-name-handler-alist))

(add-hook 'ceamx-after-init-hook #'ceamx-restore-file-name-handler-alist-h)

;; Configure load path
(dolist (subdir '("autoloads" "lisp" "lisp/core" "lisp/lib"))
  (let ((dir (expand-file-name subdir user-emacs-directory)))
    (add-to-list 'load-path dir)))

;; Load settings describing well-known paths.
(require 'ceamx-paths)

;; Configure customization file location.
;;
;; Normally, options configured in `user-init-file' won't need to be persisted
;; to `custom-file', but by default, when using package.el for package
;; management, `package-selected-packages' will always be written to
;; `custom-file' if available. See `init-package' for details.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Store packages in the designated directory.
(setq package-user-dir ceamx-packages-dir)

;; Use preferred cache directories for native-comp.
(startup-redirect-eln-cache ceamx-eln-dir)
(add-to-list 'native-comp-eln-load-path ceamx-eln-dir)

(setq native-comp-async-report-warnings-errors 'silent)
(setq native-compile-prune-cache t)

;; Don't load outdated byte-compiled files.
;;
;; NOTE: This does not handle *recompiling* the outdated files.
;; That will need to be handled during init.
;;
;; More info: <https://github.com/emacscollective/auto-compile/blob/main/README.org>
(setq load-prefer-newer t)

;; Package installation will provoke a lot of warnings from third-party
;; packages, but there's nothing we can do about those.
(setq byte-compile-warnings nil)

;; No bells.
(setq ring-bell-function #'ignore)

;; Display scratch buffer on startup.
;; TODO: replace with dashboard
(setq inhibit-startup-screen t)

;; No littering.
;; TODO: consider enabling these for TRAMP?
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Prevent X11 from taking control of visual behavior and appearance.
(setq inhibit-x-resources t)

;; Avoid expensive frame resizing.
(setq frame-inhibit-implied-resize t)

;; Allow resizing the frame to the maximum available space on the desktop.
(setq frame-resize-pixelwise t)

;; Remove some unnecessary frame elements by default.
(scroll-bar-mode -1)
(tool-bar-mode -1)

(defvar ceamx-default-frame-name "home — [ceamx]"
  "Name for the default Emacs frame.")

(defun ceamx-after-init-default-frame-name-h ()
  "Set the name for the default frame.
Simple wrapper for a call to `set-frame-name' providing
`ceamx-default-frame-name' as the NAME argument.

Intended for use as a callback on the `ceamx-after-init-hook'."
  (set-frame-name ceamx-default-frame-name))

(add-hook 'ceamx-after-init-hook #'ceamx-after-init-default-frame-name-h)