;;; init-shell-nu.el --- Nushell support             -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: local

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

;; Nushell integration and language major-mode.

;; <https://github.com/mrkkrp/nushell-mode>

;;; Code:

;; TODO: move to ~init-lang-misc~

(use-package nushell-mode

  ;; FIXME: :elpaca (nushell-mode :host github :repo "mrkkrp/nushell-mode")
  )


(provide 'init-shell-nu)
;;; init-shell-nu.el ends here
