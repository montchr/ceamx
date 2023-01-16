;;; init.el --- Personal configuration file -*- lexical-binding: t -*-

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

;;  Personal Emacs configuration file.

;;; Code:

(defvar cmx-config-dir user-emacs-directory)
(defvar cmx-cache-dir (expand-file-name "ceamx" (getenv "XDG_CACHE_HOME")))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Profile startup time.
(require 'init-benchmarking)

;; Configure customization file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq-default user-full-name "Chris Montgomery"
              user-mail-address "chris@cdom.io")

(defconst *spell-check-support-enabled* nil)
(defconst *is-a-mac* (eq system-type 'darwin))

;;; Native compilation settings
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t)
  (when (fboundp 'startup-redirect-eln-cache)
    (if (version< emacs-version "29")
        (add-to-list 'native-comp-eln-load-path (convert-standard-filename (expand-file-name "var/eln-cache/" cmx-cache-dir)))
      (startup-redirect-eln-cache (convert-standard-filename (expand-file-name "var/eln-cache/" cmx-cache-dir)))))
  (add-to-list 'native-comp-eln-load-path (expand-file-name "var/eln-cache/" cmx-cache-dir)))

(require 'init-defaults)
(require 'init-packages)

(load-theme 'modus-vivendi t)

(provide 'init)
;;; init.el ends here
