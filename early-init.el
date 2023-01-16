;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/dotfield
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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

;; Prior to Emacs 27, the `init.el' was supposed to handle the
;; initialisation of the package manager, by means of calling
;; `package-initialize'.  Starting with Emacs 27, the default
;; behaviour is to start the package manager before loading the init
;; file.

;;; Code:

(setq package-enable-at-startup nil)
(setq load-prefer-newer t)

(defun cmx/reduce-gc ()
  "Reduce the frequency of garbage collection."
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6))

(defun cmx/restore-gc ()
  "Restore the frequency of garbage collection."
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

;; Make GC more rare during init, while minibuffer is active, and
;; when shutting down. In the latter two cases we try doing the
;; reduction early in the hook.
(cmx/reduce-gc)
(add-hook 'minibuffer-setup-hook #'cmx/reduce-gc -50)
(add-hook 'kill-emacs-hook #'cmx/reduce-gc -50)

;; But make it more regular after startup and after closing minibuffer.
(add-hook 'emacs-startup-hook #'cmx/restore-gc)
(add-hook 'minibuffer-exit-hook #'cmx/restore-gc)

;; Avoid unnecessary regexp matching while loading .el files.
(defvar cmx/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun cmx/restore-file-name-handler-alist ()
  "Restores the file-name-handler-alist variable."
  (setq file-name-handler-alist cmx/file-name-handler-alist)
  (makunbound 'cmx/file-name-handler-alist))
(add-hook 'emacs-startup-hook #'cmx/restore-file-name-handler-alist)

;; LSP performance improvements.
(setenv "LSP_USE_PLISTS" "true")
(when (functionp 'json-serialize)
  (setq read-process-output-max (* 1024 1024 8)))

;; Avoid expensive frame resizing.
(setq frame-inhibit-implied-resize t)

;; Prevent early flashes of unstyled UI.
(setq-default
 mode-line-format nil
 default-frame-alist
 '((background-color . "#3F3F3F")       ; Default background color
   (bottom-divider-width . 1)           ; Thin horizontal window divider
   (foreground-color . "#DCDCCC")       ; Default foreground color
   (fullscreen . maximized)             ; Maximize the window by default
   (horizontal-scroll-bars . nil)       ; No horizontal scroll-bars
   (left-fringe . 8)                    ; Thin left fringe
   (menu-bar-lines . 0)                 ; No menu bar
   (right-divider-width . 1)            ; Thin vertical window divider
   (right-fringe . 8)                   ; Thin right fringe
   (tool-bar-lines . 0)                 ; No tool bar
   (undecorated . t)                    ; Remove extraneous X decorations
   (vertical-scroll-bars . nil)))       ; No vertical scroll-bars

(provide 'early-init)
;;; early-init.el ends here
