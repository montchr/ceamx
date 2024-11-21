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


;; [[file:config.org::*Prevent package.el from enabling all packages before init][Prevent package.el from enabling all packages before init:1]]
(setq package-enable-at-startup nil)
;; Prevent package.el from enabling all packages before init:1 ends here

;; Set up indirect init/startup hooks


;; [[file:config.org::*Set up indirect init/startup hooks][Set up indirect init/startup hooks:1]]
(defvar ceamx-after-init-hook '())
(defun ceamx-after-init-hook ()
  (run-hooks 'ceamx-after-init-hook))

(defvar ceamx-emacs-startup-hook '())
(defun ceamx-emacs-startup-hook ()
  (run-hooks 'ceamx-emacs-startup-hook))
;; Set up indirect init/startup hooks:1 ends here

;; Language servers

;; <https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process>


;; [[file:config.org::*Language servers][Language servers:1]]
(setenv "LSP_USE_PLISTS" "true")
(setq lsp-use-plists t)

;; Read JSON streams in 1MiB chunks instead of the default 4kB.
;;
;; Language server responses tend to be in the 800kB to 3MB range,
;; according to the lsp-mode documentation (linked above).
;;
;; This is a general LSP concern, not specific to any particular implementation.
(when (functionp 'json-serialize)
  (setq read-process-output-max (* 1024 1024)))
;; Language servers:1 ends here

;; Provide insight into garbage-collection activity to inform tuning decisions


;; [[file:config.org::*Provide insight into garbage-collection activity to inform tuning decisions][Provide insight into garbage-collection activity to inform tuning decisions:1]]
;; TODO: will a `init-file-debug' check work here?
(setq garbage-collection-messages t)
;; Provide insight into garbage-collection activity to inform tuning decisions:1 ends here

;; Prevent garbage-collection during init


;; [[file:config.org::*Prevent garbage-collection during init][Prevent garbage-collection during init:1]]
;; NOTE: Either use `gcmh' or make sure to reset this later.  Or else!
(setq gc-cons-threshold (* 128 1024 1024)) ; 128MiB
;; Prevent garbage-collection during init:1 ends here

;; Add directories to load path


;; [[file:config.org::*Add directories to load path][Add directories to load path:1]]
;; Configure load path
(dolist (subdir '("autoloads" "lisp" "lisp/core" "lisp/lib"))
  (let ((dir (expand-file-name subdir user-emacs-directory)))
    (add-to-list 'load-path dir)))
;; Add directories to load path:1 ends here

;; Load custom constants describing well-known paths

;; See [[*=ceamx-paths= :: common path constants]]


;; [[file:config.org::*Load custom constants describing well-known paths][Load custom constants describing well-known paths:1]]
(require 'ceamx-paths)
;; Load custom constants describing well-known paths:1 ends here

;; Configure ~custom-file~ location


;; [[file:config.org::*Configure ~custom-file~ location][Configure ~custom-file~ location:1]]
;; Normally, options configured in `user-init-file' won't need to be persisted
;; to `custom-file', but by default, when using package.el for package
;; management, `package-selected-packages' will always be written to
;; `custom-file' if available.  See `init-package' for details.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; Configure ~custom-file~ location:1 ends here

;; Store packages in the designated directory


;; [[file:config.org::*Store packages in the designated directory][Store packages in the designated directory:1]]
(setq package-user-dir ceamx-packages-dir)
;; Store packages in the designated directory:1 ends here

;; Use preferred cache directories for native-comp


;; [[file:config.org::*Use preferred cache directories for native-comp][Use preferred cache directories for native-comp:1]]
(startup-redirect-eln-cache ceamx-eln-dir)
(add-to-list 'native-comp-eln-load-path ceamx-eln-dir)
;; Use preferred cache directories for native-comp:1 ends here

;; Native compilation settings


;; [[file:config.org::*Native compilation settings][Native compilation settings:1]]
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
;; Native compilation settings:1 ends here

;; Inhibit early annoyances


;; [[file:config.org::*Inhibit early annoyances][Inhibit early annoyances:1]]
;; No bells.
(setq ring-bell-function #'ignore)

;; Allow answering yes/no questions with y/n.
(setq use-short-answers t)              ; affects `yes-or-no-p'
(setq read-answer-short t)              ; affects `read-answer' (completion)
;; Inhibit early annoyances:1 ends here

;; Frames and window-system integration

;; ;; FIXME: seems to behave inconsistently when server is running?


;; [[file:config.org::*Frames and window-system integration][Frames and window-system integration:1]]
;; Prevent X11 from taking control of visual behavior and appearance.
(setq inhibit-x-resources t)

;; Avoid expensive frame resizing.
(setq frame-inhibit-implied-resize t)

;; Allow resizing the frame to the maximum available space on the desktop.
(setq frame-resize-pixelwise t)

;; Remove some unnecessary frame elements by default.
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; `tooltip-mode' is broken for me in pgtk -- might be an Emacs bug, causes
;; constant errors when moving mouse over modeline.
;;
;; FIXME: actually, this is behaving inconsistently: disabling it does not
;; necessarily work, and toggling it off/on allows `tooltip-mode' to function
;; normally...  maybe needs to happen later in init?
(tooltip-mode -1)
;; Frames and window-system integration:1 ends here

;; Rename the default/initial frame


;; [[file:config.org::*Rename the default/initial frame][Rename the default/initial frame:1]]
(defvar ceamx-default-frame-name "home — [ceamx]"
  "Name for the default Emacs frame.")

(defun ceamx-after-init-default-frame-name-h ()
  "Set the name for the default frame.
Simple wrapper for a call to `set-frame-name' providing
`ceamx-default-frame-name' as the NAME argument.

Intended for use as a callback on the `ceamx-after-init-hook'."
  (set-frame-name ceamx-default-frame-name))

(add-hook 'ceamx-after-init-hook #'ceamx-after-init-default-frame-name-h)
;; Rename the default/initial frame:1 ends here

;; Quietize startup


;; [[file:config.org::*Quietize startup][Quietize startup:1]]
(setq inhibit-startup-screen t)
(setq initial-buffer-choice nil)
;; Quietize startup:1 ends here

(provide 'early-init)
;;; early-init.el ends here
