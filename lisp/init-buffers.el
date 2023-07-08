;;; init-buffers.el --- Buffer management -*- lexical-binding: t -*-

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

;;  Configuration for buffer management

;;; Code:

(use-feature emacs
  :init
  ;; Disable buffer line wrapping by default.
  ;; <https://www.emacswiki.org/emacs/TruncateLines>
  (set-default 'truncate-lines t))

(use-feature evil
  :general
  (+general-global-buffer
    "N" '("new" . evil-buffer-new)))

(use-feature consult
  :general
  (+general-global-buffer
    "b"  '("switch" . consult-project-buffer)
    "B"  '("any" . consult-buffer)))

(provide 'init-buffers)
;;; init-buffers.el ends here
