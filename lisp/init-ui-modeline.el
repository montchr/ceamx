;;; init-ui-modeline.el --- Modeline -*- lexical-binding: t -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chris@cdom.io>

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

;;  Modeline configuration

;;; Code:

(require 'lib-common)

(use-feature! emacs
  :config
  (setopt line-number-mode t)
  (setopt column-number-mode t))

(use-feature! time
  :config
  (setopt display-time-24hr-format t))

;; Show the name of the current function definition in the modeline.
;; TODO: on hook?
(use-feature! which-func
  :config
  (which-function-mode 1))

(provide 'init-ui-modeline)
;;; init-ui-modeline.el ends here
