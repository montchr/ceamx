;;; init-abbrevs.el --- Abbrevs support  -*- lexical-binding: t;  -*-

;; Copyright (c) 2024  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0

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
;;; Code:

(setopt abbrev-file-name (locate-user-emacs-file "abbrev-defs"))
(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))
(setopt abbrev-suggest t)
(defconst ceamx-abbrev-prefix-regexp
  (rx (or bol
          (1+ (any "\t ")))
      (group-n 1
        (or (seq (any ":;_") (0+ nonl))
            (0+ nonl)))))
(after! abbrev
  (abbrev-table-put global-abbrev-table :regexp ceamx-abbrev-prefix-regexp)

  (with-eval-after-load 'text-mode
    (abbrev-table-put text-mode-abbrev-table :regexp ceamx-abbrev-prefix-regexp))

  (with-eval-after-load 'org
    (abbrev-table-put org-mode-abbrev-table :regexp ceamx-abbrev-prefix-regexp)))
(dolist (hook '(text-mode-hook prog-mode-hook git-commit-mode-hook))
  (add-hook hook #'abbrev-mode))

(provide 'init-abbrevs)
;;; init-abbrevs.el ends here
