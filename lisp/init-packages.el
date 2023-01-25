;;; init-packages.el --- Package system initialization -*- lexical-binding: t -*-

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

;;  Configuration for Emacs package management.

;;; Code:

;;
;;; === ELPACA =================================================================
;;  <https://github.com/progfolio/elpaca>

;; `elpaca' recipes seem to follow the `straight.el' format...
;; (maybe? does not seem to be documented entirely...):
;;
;; <https://github.com/radian-software/straight.el/tree/develop#the-recipe-format>
;;
;; Sometimes packages might include additional subpackages in their repo. 
;; Examples I've encountered: `evil-collection', `corfu', `vertico'.
;; These additional files can be copied to the package destination
;; in addition to the default package file(s) by declaring them in a recipe
;; under the `:files' property. For example, given the element
;; `:files (:defaults "snippets")', all files under the `snippets' subdirectory
;; would also be copied. See the recipe format docs for details.

;; Configurate package/build directories
(defvar elpaca-directory (expand-file-name "elpaca/" +path-packages-dir))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))

;; Bootstrap
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :build (:not elpaca--activate-package)))
(when-let ((repo  (expand-file-name "repos/elpaca/" elpaca-directory))
           (build (expand-file-name "elpaca/" elpaca-builds-directory))
           (order (cdr elpaca-order))
           ((add-to-list 'load-path (if (file-exists-p build) build repo)))
           ((not (file-exists-p repo))))
  (condition-case-unless-debug err
      (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
               ((zerop (call-process "git" nil buffer t "clone"
                                     (plist-get order :repo) repo)))
               (default-directory repo)
               ((zerop (call-process "git" nil buffer t "checkout"
                                     (or (plist-get order :ref) "--")))))
          (progn
            (byte-recompile-directory repo 0 'force)
            (require 'elpaca)
            (and (fboundp 'elpaca-generate-autoloads)
                 (elpaca-generate-autoloads "elpaca" repo))
            (kill-buffer buffer))
        (error "%s" (with-current-buffer buffer (buffer-string))))
    ((error)
     (warn "%s" err)
     (delete-directory repo 'recursive))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Use Evil bindings in status and UI maps
(with-eval-after-load 'evil
  (with-eval-after-load 'elpaca-ui
    (evil-make-intercept-map elpaca-ui-mode-map)))


;;
;;; === USE-PACKAGE ============================================================
;;  <https://github.com/jwiegley/use-package>

(defmacro use-feature (name &rest args)
  "Like `use-package' but accounting for asynchronous installation.
NAME and ARGS are in `use-package'."
  (declare (indent defun))
  `(elpaca nil (use-package ,name
                 :ensure nil
                 ,@args)))

;; Install use-package.
(elpaca use-package (require 'use-package))

;; `use-package' debug mode.
(setq init-file-debug t)
(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

(provide 'init-packages)
;;; init-packages.el ends here
