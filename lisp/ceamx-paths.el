;;; ceamx-paths.el --- Ceamx :: Paths                -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: internal

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

;; Commonly-used paths.

;;; Code:

;;;; Functions

(defun ceamx-format-version-subdir (parent)
  "Return a path-like string for a subdirectory of PARENT based on the current Emacs version."
  (format "%s/%s.%s/"
    parent
    emacs-major-version
    emacs-minor-version))

;;;; Base Variables

(defconst ceamx-lisp-dir
  (locate-user-emacs-file "lisp")
  "Absolute path to the lisp directory.")

(defconst ceamx-site-lisp-dir
  (locate-user-emacs-file "site-lisp")
  "Absolute path to the site-lisp directory.")

(defconst ceamx-home-dir
  (file-name-as-directory (getenv "HOME"))
  "Path to user home directory.")

(defconst ceamx-xdg-config-dir
  (file-name-as-directory
   (or (getenv "XDG_CONFIG_HOME")
       (concat ceamx-home-dir ".config"))))

(defconst ceamx-xdg-data-dir
  (file-name-as-directory
   (or (getenv "XDG_DATA_HOME")
       (concat ceamx-home-dir ".local/share"))))

(defconst ceamx-xdg-cache-dir
  (file-name-as-directory
   (or (getenv "XDG_CACHE_HOME")
       (concat ceamx-home-dir ".cache"))))

;;;; Storage

(defconst ceamx-storage-dir
  (file-name-as-directory (concat ceamx-xdg-data-dir "ceamx/"))
  "The root directory for local Emacs files.
Use this as permanent storage for files that are safe to share
across systems.")

(defconst ceamx-cache-dir
  (file-name-as-directory (concat ceamx-xdg-cache-dir "ceamx/")))

(defconst ceamx-packages-dir
  (file-name-as-directory
   (concat ceamx-storage-dir (ceamx-format-version-subdir "packages"))))

;;;; User

(defconst ceamx-reading-dir
  (expand-file-name "Documents/reading/" ceamx-home-dir)
  "The user directory for reading materials.")

(defconst ceamx-notes-dir
  (expand-file-name "Documents/notes/" ceamx-home-dir)
  "The user directory for notes.")

(defconst ceamx-biblio-dir
  (expand-file-name "Documents/notes/biblio/" ceamx-home-dir)
  "The user directory for bibliographies.")

(defconst ceamx-projects-dir
  (file-name-as-directory
    (or (getenv "XDG_PROJECTS_DIR")
        (concat ceamx-home-dir "Projects")))
  "The root directory for projects.")


;;;; Feature

(defconst ceamx-templates-dir
  (expand-file-name "templates" user-emacs-directory))

(defconst ceamx-feeds-dir
  (expand-file-name "feeds" ceamx-reading-dir))

(provide 'ceamx-paths)
;;; ceamx-paths.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
