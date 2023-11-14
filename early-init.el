;;; early-init.el --- Early Init File

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

;; We don't want to use `package.el'. Considering the init behavior
;; described in the Commentary, we need to disable `package.el' immediately.
(setq package-enable-at-startup nil)

(setq inhibit-default-init nil)
(setq native-comp-async-report-warnings-errors nil)
(setq load-prefer-newer t)

;; Skip expensive regular expression searches in file name handlers.
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(load (concat (file-name-directory load-file-name)
              "lisp/config-paths")
      nil (not init-file-debug))

(load (expand-file-name "dotfield-early-init.el" user-emacs-directory))
(require 'dotfield-early-init)

(defalias 'cmx-init-hook 'after-init-hook)

;;
;;; Startup performance tuning
;;

;; Relocate the native-comp cache during early-init.
(startup-redirect-eln-cache (convert-standard-filename (expand-file-name "eln/" cmx-var-dir)))
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln/" cmx-var-dir))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1)

(defun +gc-after-focus-change ()
  "Run garbage collection when frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))
(defun +reset-init-values ()
  "Restore sensible settings after initialization."
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setq file-name-handler-alist default-file-name-handler-alist
           gc-cons-percentage 0.1
           gc-cons-threshold 100000000)
     ;; TODO: debug mode flag
     ;; (message "gc-cons-threshold & file-name-handler-alist restored")
     (when (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function #'+gc-after-focus-change)))))
;; (with-eval-after-load 'elpaca
;;   (add-hook 'elpaca-after-init-hook '+reset-init-values))
(add-hook 'cmx-init-hook #'+reset-init-values)

;; LSP performance improvements.
(setenv "LSP_USE_PLISTS" "true")
(when (functionp 'json-serialize)
  (setq read-process-output-max (* 1024 1024 8)))

;; Ignore Xorg resources.
(advice-add #'x-apply-session-resources :override #'ignore)

;; Inhibit annoyances.
(setq ring-bell-function #'ignore
      inhibit-startup-screen t)

(provide 'early-init)
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; early-init.el ends here
