;;; lib-completion.el --- Completion helpers  -*- lexical-binding: t;  -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@proton.me>

;; Author: Chris Montgomery <chmont@proton.me>
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

(defvar +dabbrev-friend-buffer-size-max (* 1 1024 1024) ; 1 MB
  "Size limit for a buffer to be scanned for dynamic abbreviations.")

(defun +dabbrev-friend-buffer-p (buf)
  "Whether to consider BUF a `dabbrev' friend buffer."
  (< (buffer-size buf) +dabbrev-friend-buffer-size-max))

(provide 'lib-completion)
;;; lib-completion.el ends here
