;;; ceamx-paths.el --- Common paths variables        -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords:

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

;; Define variables pointing to commonly-used paths.

;; Path variables relating to specific modules should be defined in a
;; feature-specific file i.e. `config-<module>'.

;;; Code:

;;
;;; Functions

(defun ceamx-format-version-subdir (parent)
  "Return a path-like string for a subdirectory of PARENT based on the current Emacs version."
  (format "%s/%s.%s/"
    parent
    emacs-major-version
    emacs-minor-version))

;;
;;; Variables

(defvar ceamx-site-lisp-dir
  (concat user-emacs-directory "site-lisp/")
  "Absolute path to the site-lisp directory.")

(defvar ceamx-home-dir (file-name-as-directory (getenv "HOME"))
  "Path to user home directory.")

(defvar ceamx-xdg-config-dir
  (file-name-as-directory
   (or (getenv "XDG_CONFIG_HOME")
       (concat ceamx-home-dir ".config"))))

(defvar ceamx-xdg-cache-dir
  (file-name-as-directory
   (or (getenv "XDG_CACHE_HOME")
       (concat ceamx-home-dir ".cache"))))

(defvar ceamx-config-dir ceamx-xdg-config-dir
  "The root directory for personal configurations.")

;; TODO: rename to something like `ceamx-storage-dir' to reduce confusion
(defvar ceamx-local-dir
  (concat ceamx-xdg-cache-dir "ceamx/")
  "The root directory for local Emacs files.
Use this as permanent storage for files that are safe to share
across systems.")

(defvar ceamx-etc-dir (concat ceamx-local-dir "etc/")
  "Directory for non-volatile storage.
Use this for files that don't change much, like servers binaries,
external dependencies or long-term shared data.")

(defvar ceamx-var-dir (concat ceamx-local-dir "var/")
  "Directory for volatile storage.
Use this for files that change often, like data and cache files.")

;; FIXME: avoid usage of `expand-file-name', which is incorrect -- read its
;; documentation / the manual section.  `convert-standard-filename' may also be
;; removed in this context.  the latter is likely better off used as needed, and
;; closer to the usage rather than in this declaration.  this declaration *is*
;; the standard Unix-like filename expected by `convert-standard-filename'.
(defvar ceamx-eln-dir (convert-standard-filename
                       (file-name-as-directory
                        (expand-file-name "eln/" ceamx-var-dir)))
  "Directory for natively-compiled eln files.")

(defvar ceamx-packages-dir
  (expand-file-name (ceamx-format-version-subdir "packages")
    ceamx-local-dir)
  "Where packages are stored.
Intended for setting the value of `package-user-dir' or the
equivalent settings for third-party package managers.

Packages will be stored in subdirectories based on the current
Emacs version to prevent bytecode incompatibility.")

(defconst ceamx-templates-dir
  (file-name-as-directory (file-name-concat user-emacs-directory "templates"))
  "Directory for user-defined expandable templates.
Templates, in this sense, refer to the primary focus of packages
like \"tempo\", \"tempel\", and \"yasnippet\".  Care should be
taken to distinguish this usage of the term \"template\" from the
similar but essentially different concept of a \"snippet\".  A
snippet, in our usage, refers to a /non-expandable/ and /static/
block of saved text lacking a \"templatable\" characteristic.")

(provide 'ceamx-paths)
;;; ceamx-paths.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
