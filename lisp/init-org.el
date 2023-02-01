;;; init-org.el --- org-mode initialisation -*- lexical-binding: t -*-

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

;;  Configuration for `org-mode'.

;;; Code:

(elpaca-use-package org
  :general
  (global-definer
    "X"  '(org-capture :which-key "capture..."))
  (global-leader
    "a" '(org-archive-subtree :which-key "archive subtree")
    "E" '(org-export-dispatch :which-key "export...")
    "l" '(:ignore true :wk "link")
    "l l" '(org-insert-link :wk "insert link")
    "l s" '(org-store-link :wk "store link")
    "r" '(org-refile :wk "refile...")
    "p" '(org-priority :wk "priority")
    "q" '(org-set-tags-command :wk "tag")
    "s" '(org-sort :wk "sort")
    "t" '(:ignore true :wk "todo")
    "t t" '(org-todo :wk "heading todo")
    "t s" '(org-schedule :wk "schedule")
    "t d" '(org-deadline :wk "deadline")
    "x" '(org-toggle-checkbox :wk "toggle checkbox"))

  :hook ((org-mode . prettify-symbols-mode)
         (org-mode . visual-line-mode)))

(provide 'init-org)
;;; init-org.el ends here
