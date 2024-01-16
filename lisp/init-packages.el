;;; init-packages.el --- Package management setup    -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Initialize package.el and `use-package' (from bundled version).

;;; Code:

(require 'cl-lib)

(require 'ceamx-paths)
(require 'lib-common)

(require 'package)

;; via <https://github.com/purcell/emacs.d/blob/45dc1f21cce59d6f5d61364ff56943d42c8b8ba7/lisp/init-elpa.el#L69-L86>
;; (defvar ceamx-selected-packages nil
;;   "Track the packages installed by `ceamx-require-package'.
;; This aims to avoid potential issues with
;; `package-selected-packages'.")

;; (defun ceamx-note-selected-package-a (oldfun package &rest args)
;;   "If OLDFUN reports PACKAGE was successfully installed, note that fact.
;; The package name is noted by adding it to
;; `ceamx-selected-packages'.  This function is used as an
;; advice for `require-package', to which ARGS are passed."
;;   (let ((available (apply oldfun package args)))
;;     (prog1
;;         available
;;       (when available
;;         (add-to-list 'ceamx-selected-packages package)))))

;; (advice-add 'ceamx-require-package :around 'ceamx-note-selected-package-a)

;;; Prevent `seq' dependency hell induced by `magit' changes.

;; Work around an issue in Emacs 29 where seq gets implicitly reinstalled via
;; the rg -> transient dependency chain, but fails to reload cleanly due to not
;; finding seq-25.el, breaking first-time start-up.
;;
;; Source: <https://github.com/purcell/emacs.d/blob/45dc1f21cce59d6f5d61364ff56943d42c8b8ba7/lisp/init-elpa.el#L89-L100>
;; See: <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=67025>
;;
;; TODO: make sure this condition catches unstable versions of emacs 29.1
;; (when (string= "29.1" emacs-version)
;;   (defadvice! ceamx-reload-previously-loaded-with-updated-load-path-a (orig pkg-desc)
;;     "Update the ORIG load-path with PKG-DESC directories.
;; Intended as a workaround for `seq' dependency hell caused by
;; recent `magit' changes."
;;     :around 'package--reload-previously-loaded
;;     (let ((load-path (cons (package-desc-dir pkg-desc) load-path)))
;;       (funcall orig pkg-desc))))

;; (when (fboundp 'package--save-selected-packages)
;;   (ceamx-package 'seq)
;;   (def-hook! ceamx-merge-selected-package-lists-a () 'after-init-hook
;;     "Merge the package selections from `package' and `ceamx-selected-packages'."
;;     (package--save-selected-packages
;;       (seq-uniq (append ceamx-selected-packages package-selected-packages)))))

;; Package installation will provoke a lot of these, but that's the package
;; developers' problem, not ours.
(setq byte-compile-warnings nil)

;; Allow upgrading of builtin packages available in ELPA.
;;
;; This is *required* for `magit' currently, as it loads a version of `seq'
;; which is only available in Emacs 30.
(setopt package-install-upgrade-built-in t)

(setopt package-native-compile t)

;; Also read: <https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/>
(setopt package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0).
(setopt package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

;; Begin the process of installing and `require'ing packages selected via
;; `package-install' or a function which invokes it, including
;; `ceamx-require-package' or `ceamx-package'.
;;
;; When a package is selected for installation by `package-install', its
;; specification is added to the queue stored in `package-selected-packages'.
;; Once called, `package-initialize' will process the queued packages.
;;
;; If relying on `use-package' for package initialization, `package-initialize'
;; should not be called. (TODO: verify this)
;; (package-initialize)

;;
;;; `use-package' :: <https://github.com/jwiegley/use-package>
;;  <https://www.gnu.org/software/emacs/manual/html_mono/use-package.html>

;; (ceamx-package use-package)

;; When non-nil, improves performance and effectiveness of byte-compilation,
;; but decreases introspectability.
;; If byte-compiling user configurations, this should be non-nil.
(setopt use-package-expand-minimally nil)

;; NOTE: If a `use-package' declaration should not use `:ensure', use `use-feature!'
;; instead, which already handles that.
(setopt use-package-always-ensure t)

;;; Support for Emacs init introspection.
(when (bound-and-true-p init-file-debug)
  ;; NOTE: Most of the options configured below require that the `use-package'
  ;;       library is explicitly `require'd in files where its macro is
  ;;       invoked, for various reasons. See their docstrings for more info.
  (require 'use-package)
  (setopt use-package-expand-minimally nil)
  (setopt use-package-verbose t)
  (setopt use-package-compute-statistics t))

;;;; Initialize packages adding `use-package' keywords.

;; NOTE: `blackout' is still useful even without `use-package'
(use-package blackout
  :demand t
  :autoload (blackout))

(provide 'init-packages)
;;; init-packages.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
