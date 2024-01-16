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

;; Prior to Emacs 27, the `init.el' was supposed to handle the initialisation of
;; `package.el', by means of calling `package-initialize'. Starting with Emacs
;; 27, the default behavior is to start `package.el' before loading init.el
;; after early-init.el.

;;; Links:

;; Helpful guide to early-init configuration for package management:
;; <https://old.reddit.com/r/emacs/comments/np6ey4/how_packageel_works_with_use_package/>
;; See also the Commentary in `init-packages' for more context.

;;; Code:

;; Prevent package.el from enabling all packages before init.
;;
;; When nil, `package-initialize' must be invoked in the init process prior to
;; `require'ing any packages installed with `package-install'.
;;
;; When non-nil, there is no need to invoke `package-initialize'.
(setq package-enable-at-startup nil)

;;
;;; Performance

;; Performance improvements for language server JSON-RPC (LSP).
(when (functionp 'json-serialize)
  (setq read-process-output-max (* 1024 1024 8)))

;;; Minimize garbage collection during startup.

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

;;; Simplify filename pattern-matching during init.
;;  <https://github.com/jwiegley/dot-emacs/blob/79bc2cff3a28ecd1a315609bbb607eb4ba700f76/init.org#during-loading-of-this-module-clear-file-name-handler-alist>
;;  <https://old.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/>

(defvar cmx-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun cmx-restore-file-name-handler-alist-h ()
  "Restore the original value of the `file-name-handler-alist' variable.
Intended for use as a callback on `after-init-hook'."
  (setq file-name-handler-alist cmx-file-name-handler-alist)
  (makunbound 'cmx-file-name-handler-alist))

(add-hook 'after-init-hook #'cmx-restore-file-name-handler-alist-h)
(with-eval-after-load 'elpaca
  (remove-hook 'after-init-hook #'cmx-restore-file-name-handler-alist-h)
  (add-hook 'elpaca-after-init-hook #'cmx-restore-file-name-handler-alist-h))

;;
;;; Directories:

;;; Configure load path.
(dolist (subdir '("autoloads" "lisp" "lisp/core" "lisp/lib"))
  (let ((dir (expand-file-name subdir user-emacs-directory)))
    (add-to-list 'load-path dir)))

;; Load settings describing well-known paths.
(require 'ceamx-paths)

;; Store packages in the designated directory.
(setopt package-user-dir cmx-packages-dir)

;; Use preferred cache directories for native-comp.
(startup-redirect-eln-cache cmx-eln-dir)
(add-to-list 'native-comp-eln-load-path cmx-eln-dir)

;;
;;; Native compilation:

(setq native-comp-async-report-warnings-errors 'silent)
(setq native-compile-prune-cache t)

;; Don't load outdated byte-compiled files. This should not even be an option.
(setq load-prefer-newer t)

;;
;;; Inhibit annoyances:

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
(add-hook 'after-init-hook (lambda () (set-frame-name "home")))

(provide 'early-init)
;;; early-init.el ends here
