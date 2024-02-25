;;; init-lang-shell.el --- Shell script language support  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
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

;; NOTE: Make sure `flycheck-mode' is not enabled in shell script buffers, as
;; `flymake' will handle it just fine.

;;; Code:

(require 'lib-common)

(use-feature! emacs
  :config
  ;; Make files executable if their first line has a shebang.
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p))

(use-feature! eglot
  :config
  (add-hook 'sh-mode-hook #'eglot-ensure)
  (add-hook 'bash-ts-mode-hook #'eglot-ensure)
  (add-to-list 'eglot-server-programs '((sh-mode bash-ts-mode) . ("bash-language-server" "start"))))

(use-feature! flymake
  :config
  (add-hook 'sh-mode-hook #'flymake-mode)
  (add-hook 'bash-ts-mode-hook #'flymake-mode))

(provide 'init-lang-shell)
;;; init-lang-shell.el ends here
