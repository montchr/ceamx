;;; lib-ui-treemacs.el --- Treemacs library functions+macros  -*- lexical-binding: t; -*-

;; Copyright (c) 2023  Chris Montgomery <chris@cdom.io>
;; Copyright (c) 2014-2022  Henrik Lissner
;; SPDX-License-Identifier: GPL-3.0-or-later AND MIT

;; Author: Henrik Lissner
;;         Chris Montgomery <chris@cdom.io>

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

;; Helpers for Treemacs.

;;; Code:

(autoload 'treemacs-current-visibility "treemacs")
(autoload 'treemacs-get-local-window "treemacs")
(autoload 'treemacs-add-and-display-current-project "treemacs")

;; via <https://github.com/doomemacs/doomemacs/blob/7a7503045850ea83f205de6e71e6d886187f4a22/modules/ui/treemacs/autoload.el>
;;;###autoload
(defun cmx/treemacs/toggle ()
  "Initialize or toggle treemacs.

Ensures that only the current project is present and all other projects have
been removed.

Use `treemacs' command for original functionality."
  (interactive)
  (require 'treemacs)
  (pcase (treemacs-current-visibility)
    (`visible (delete-window (treemacs-get-local-window)))
    (_ (treemacs-add-and-display-current-project))))

(provide 'lib-ui-treemacs)
;;; lib-ui-treemacs.el ends here
