;;; config-package.el --- Settings relating to package management  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

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

;;; Sources:

;; - <https://git.sr.ht/~protesilaos/dotfiles/tree/e21affc0153e556e06a28813efb252c7757b6aff/item/emacs/.emacs.d/init.el>

;;; Code:

(defvar cmx-loaded-packages nil)

(defcustom cmx-exclude-packages nil
  "List of package names to exclude from loading.
This instructs the relevant macros to not `require' the given
package."
  :group 'ceamx
  :type '(repeat symbol))


(provide 'config-package)
;;; config-package.el ends here
