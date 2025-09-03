;;; ceamx-dev-loader.el --- Development settings for loading Ceamx  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery
;; Copyright (C) 2020-2024  Nicholas Vollmer

;; Author: Chris Montgomery <chmont@proton.me>
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

;; <https://github.com/progfolio/.emacs.d/blob/509cbaf74f30b55e87855e0dd57d9c6309826083/init-dev.el>

;;;; Usage

;; To be loaded at the top of config.org with:

;; # -*- eval: (load-file "./ceamx-dev-loader.el");   -*-

;;; Code:

(require 'auto-tangle-mode)

(setq-local org-confirm-babel-evaluate nil)

(auto-tangle-mode 1)

(defun +auto-tangle-reload-init-h ()
  "Reload the init file and process any Elpaca queues.
Intended for use as a hook function on
`auto-tangle-after-tangle-hook'."
  (defvar elpaca-log-functions)
  (declare-function elpaca-process-queues "elpaca")
  (let ((elpaca-log-functions nil))
    (load-file (file-name-concat user-emacs-directory "init.el"))
    (elpaca-process-queues)))

(add-hook 'auto-tangle-after-tangle-hook #'+auto-tangle-reload-init-h)

(eldoc-mode 1)

(provide 'ceamx-dev-loader)
;;; ceamx-dev-loader.el ends here
