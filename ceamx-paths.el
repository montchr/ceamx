;;; ceamx-paths.el --- Common paths variables        -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

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

(defvar cmx-site-lisp-dir
  (concat user-emacs-directory "site-lisp/")
  "Absolute path to the site-lisp directory.")

(defvar cmx-home-dir (file-name-as-directory (getenv "HOME"))
  "Path to user home directory.")

(defvar cmx-config-dir
  (file-name-as-directory
   (or (getenv "XDG_CONFIG_HOME")
       (concat cmx-home-dir ".config")))
  "The root directory for personal configurations.")

(defvar cmx-local-dir
  (concat
   (file-name-as-directory
    (or (getenv "XDG_CACHE_HOME")
        (concat cmx-home-dir ".cache")))
   "ceamx/")
  "The root directory for local Emacs files.
Use this as permanent storage for files that are safe to share
across systems.")

(defvar cmx-etc-dir (concat cmx-local-dir "etc/")
  "Directory for non-volatile storage.
Use this for files that don't change much, like servers binaries,
external dependencies or long-term shared data.")

(defvar cmx-var-dir (concat cmx-local-dir "var/")
  "Directory for volatile storage.
Use this for files that change often, like data and cache files.")

(defvar cmx-eln-dir (convert-standard-filename
                      (file-name-as-directory
                        (expand-file-name "eln/" cmx-var-dir)))
  "Directory for natively-compiled eln files.")

(defvar cmx-packages-dir
  (expand-file-name (format "packages/%s.%s/"
                      emacs-major-version
                      emacs-minor-version)
    cmx-local-dir)
  "Where packages are stored.
See also `package-user-dir'.
Packages will be stored in subdirectories based on the current
Emacs version to prevent bytecode incompatibility.")

(provide 'ceamx-paths)
;;; ceamx-paths.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
