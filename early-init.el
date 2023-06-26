;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/ceamx
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

;;
;;; === PATHS ======================================================================================
;;

(defconst +path-home-dir (file-name-as-directory (getenv "HOME"))
  "Path to user home directory.")

(defconst +path-config-dir
  (file-name-as-directory
   (or (getenv "XDG_CONFIG_HOME")
       (concat +path-home-dir ".config")))
  "The root directory for personal configurations.")

(defconst +path-emacs-dir user-emacs-directory)

(defconst +path-local-dir
  (concat
   (file-name-as-directory
    (or (getenv "XDG_CACHE_HOME")
        (concat +path-home-dir ".cache")))
   "ceamx/")
  "The root directory for local Emacs files.
Use this as permanent storage for files that are safe to share
across systems.")

(defconst +path-etc-dir (concat +path-local-dir "etc/")
  "Directory for non-volatile storage.
Use this for files that don't change much, like servers binaries,
external dependencies or long-term shared data.")

(defconst +path-var-dir (concat +path-local-dir "var/")
  "Directory for volatile storage.
Use this for files that change often, like data and cache files.")

(defconst +path-packages-dir
  (expand-file-name (format "packages/%s.%s/"
                            emacs-major-version
                            emacs-minor-version)
                    +path-local-dir)
  "Where packages are stored.")

(defconst +path-projects-dir
  (file-name-as-directory
   (or (getenv "XDG_PROJECTS_HOME")
       (concat +path-home-dir "Developer")))
  "The root directory for projects.")

;;
;;; === STARTUP PERFORMANCE TUNING =================================================================
;;

;; Relocate the native-comp cache during early-init.
(startup-redirect-eln-cache (convert-standard-filename (expand-file-name "eln/" +path-var-dir)))
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln/" +path-var-dir))

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
(with-eval-after-load 'elpaca
  (add-hook 'elpaca-after-init-hook '+reset-init-values))

;; LSP performance improvements.
(setenv "LSP_USE_PLISTS" "true")
(when (functionp 'json-serialize)
  (setq read-process-output-max (* 1024 1024 8)))

;; Avoid expensive visual elements before UI initialization.
(setq frame-inhibit-implied-resize t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)

;; Prevent early flashes of unstyled UI.
(setq-default
 ;; mode-line-format nil
 default-frame-alist
 '((background-color . "#3F3F3F")       ; Default background color
   (bottom-divider-width . 1)           ; Thin horizontal window divider
   (foreground-color . "#DCDCCC")       ; Default foreground color
   (left-fringe . 8)                    ; Thin left fringe
   (right-divider-width . 1)            ; Thin vertical window divider
   (right-fringe . 8)                   ; Thin right fringe
   (undecorated . t)))                  ; Remove extraneous OS window system decorations

;; Basic font configuration.
;; `fontaine' will handle fonts once loaded in `init-theme'.
(push '(font . "Iosevka") default-frame-alist)
(set-face-font 'default "Iosevka")
(set-face-font 'variable-pitch "Inter")
(copy-face 'default 'fixed-pitch)

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

