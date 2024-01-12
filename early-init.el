;;; early-init.el --- Early Init File  -*- no-byte-compile: t; -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

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

;; Prior to Emacs 27, the `init.el' was supposed to handle the
;; initialisation of `package.el', by means of calling
;; `package-initialize'. Starting with Emacs 27, the default
;; behavior is to start `package.el' before loading the init
;; file.

;;; Code:

;; Disable package.el
(setq package-enable-at-startup nil)

;;; PERF: Minimize garbage collection during startup:

(defun cmx-gc-reduce-freq ()
  "Reduce the frequency of garbage collection."
  (setq gc-cons-threshold most-positive-fixnum)
  (setq gc-cons-percentage 0.6))

(defun cmx-gc-restore-freq ()
  "Restore the frequency of garbage collection."
  ;; FIXME: these appear to be arbitrary values
  (setq gc-cons-threshold 16777216)
  (setq gc-cons-percentage 0.1))

;; Make GC more rare during init (i.e. right now), while minibuffer is active,
;; and when shutting down. In the latter two cases, we try doing the reduction
;; early in the hook.
(cmx-gc-reduce-freq)
(add-hook 'minibuffer-setup-hook #'cmx-gc-reduce-freq -50)
(add-hook 'kill-emacs-hook #'cmx-gc-reduce-freq -50)

;; But make it more regular after startup and after closing minibuffer.
(add-hook 'emacs-startup-hook #'cmx-gc-restore-freq)
(with-eval-after-load 'elpaca
  (remove-hook 'emacs-startup-hook #'cmx-gc-restore-freq)
  (add-hook 'elpaca-after-init-hook #'cmx-gc-restore-freq))
(add-hook 'minibuffer-exit-hook #'cmx-gc-restore-freq)

;;
;;; Directories:

;; Load settings describing well-known paths.
(load (concat (file-name-directory load-file-name)
              "ceamx-paths")
      nil (not init-file-debug))
;; (require 'ceamx-paths
;;   (concat (file-name-directory load-file-name)
;;                         "ceamx-paths.el"))

;; Use preferred cache directories for native-comp.
(startup-redirect-eln-cache cmx-eln-dir)
(add-to-list 'native-comp-eln-load-path cmx-eln-dir)

;;
;;; PERF: Avoid complex regexp matching in load path during startup.

(defvar cmx-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun cmx-restore-file-name-handler-alist-h ()
  "Restore the original value of the `file-name-handler-alist' variable."
  (setq file-name-handler-alist cmx-file-name-handler-alist)
  (makunbound 'cmx-file-name-handler-alist))

(add-hook 'emacs-startup-hook #'cmx-restore-file-name-handler-alist-h)
(with-eval-after-load 'elpaca
  (remove-hook 'emacs-startup-hook #'cmx-restore-file-name-handler-alist-h)
  (add-hook 'elpaca-after-init-hook #'cmx-restore-file-name-handler-alist-h))

;;
;;; Native compilation:

(setq native-comp-async-report-warnings-errors 'silent)
(setq native-compile-prune-cache t)

;; Don't load outdated byte-compiled files. This should not even be an option.
(setq load-prefer-newer t)

;;
;;; Inhibit annoyances:

;; Performance improvements for language server JSON-RPC (LSP).
(when (functionp 'json-serialize)
  (setq read-process-output-max (* 1024 1024 8)))

;; No bells.
(setq ring-bell-function #'ignore)

;; Display scratch buffer on startup.
(setq inhibit-startup-screen t)

;; No littering.
(setq make-backup-files nil)
(setq create-lockfiles nil)

;;
;;; Frames:

;; Prevent X11 from taking control of visual behavior and appearance.
(setq inhibit-x-resources t)

;; Avoid expensive frame resizing.
(setq frame-inhibit-implied-resize t)

;; Allow resizing the frame to the maximum available space on the desktop.
(setq frame-resize-pixelwise t)

;; There is no place like Emacs.
(add-hook 'elpaca-after-init-hook (lambda () (set-frame-name "home")))

(provide 'early-init)
;;; early-init.el ends here
