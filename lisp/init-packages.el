;;; init-packages.el --- Package system initialization -*- lexical-binding: t -*-

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

;;  Configuration for Emacs package management.

;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(customize-set-variable 'package-archive-priorities
                        '(("gnu"    . 99)   ; Prefer: GNU packages
                          ("nongnu" . 80)   ; Fallback: Use non-GNU packages
                          ("stable" . 70)   ; Prefer: MELPA stable releases
                          ("melpa"  . 0)))  ; Fallback: MELPA rolling releases

(customize-set-variable 'package-user-dir (expand-file-name "etc/elpa/" cmx-cache-dir))

(provide 'init-packages)
;;; init-packages.el ends here
