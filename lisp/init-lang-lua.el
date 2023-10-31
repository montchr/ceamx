;;; init-lang-lua.el --- Lua language support        -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords: languages, local

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

;; Lua language support.

;;;; Sources:

;; - <https://github.com/purcell/emacs.d/blob/28194a035ca9a259030ba7ef58089561078c4893/lisp/init-lua.el>

;;; Code:

(require 'lib-common)

(use-package lua-mode
  :config
  (setq-default lua-indent-level 2)
  (after! [reformatter]
    (reformatter-define lua-format
      :program "lua-format"
      :args '("--indent-width=2" "--no-use-tab"))))

(provide 'init-lang-lua)
;;; init-lang-lua.el ends here
