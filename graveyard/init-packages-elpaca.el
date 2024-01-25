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

;; TODO: <https://github.com/ashlineldridge/.config/blob/main/emacs/elpaca-bootstrap.el>

;;; Code:

(require 'cl-lib)

(require 'ceamx-paths)

(require 'lib-common)

;; Package installation will provoke a lot of these, but that's the package
;; developers' problem, not ours.
(setq byte-compile-warnings nil)

;;; elpaca

;; Prevent "unable to determine `elpaca-core-date'" warnings on init.
;; <https://github.com/progfolio/elpaca/issues/222>
;; This *must* be set prior to loading `elpaca' in order to take effect.
;; This is the recommended value from the maintainer.
(setq elpaca-core-date '(20231211))

(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" ceamx-packages-dir))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;
;;; `use-package' :: <https://github.com/jwiegley/use-package>
;;  <https://www.gnu.org/software/emacs/manual/html_mono/use-package.html>

(elpaca use-package
  (require 'use-package))
(elpaca-wait)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;; When non-nil, improves performance and effectiveness of byte-compilation,
;; but decreases introspectability.
;; If byte-compiling user configurations, this should be non-nil.
(setopt use-package-expand-minimally nil)

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

(elpaca-wait)

(provide 'init-packages)
;;; init-packages.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
