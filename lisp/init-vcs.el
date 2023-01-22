;;; init-vcs.el --- Git/VCS -*- lexical-binding: t -*-

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

;;  Configurations for git + magit.
;;  And other version control systems, if they exist...

;;; Code:

;; Follow symlinks.
(setq vc-follow-symlinks t)

;; FIXME: evil bindings unavailable! prob an issue with general
(elpaca-use-package magit
  :after (general)

  :general
  (+general-global-git/version-control
    "g"  'magit-status
    "b"  'magit-branch
    "B"  'magit-blame
    "c"  'magit-clone
    "f"  '(:ignore t :which-key "file")
    "ff" 'magit-find-file
    "fh" 'magit-log-buffer-file
    "i"  'magit-init
    "L"  'magit-list-repositories
    "m"  'magit-dispatch
    "S"  'magit-stage-file
    "s"  'magit-status
    "U"  'magit-unstage-file)

  :config
  ;; Close transient with ESC or q
  (transient-bind-q-to-quit)
  (define-key transient-map [escape] #'transient-quit-one))

(provide 'init-vcs)
;;; init-vcs.el ends here
