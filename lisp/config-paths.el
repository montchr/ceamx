;;; config-paths.el --- Common paths variables        -*- lexical-binding: t; -*-

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

;;

;;; Code:

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

(defconst cmx-path-notes-dir
  (file-name-as-directory
   (concat +path-home-dir "Documents/notes")))

(provide 'config-paths)
;;; config-paths.el ends here
