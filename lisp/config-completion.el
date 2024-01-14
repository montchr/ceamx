;;; config-completion.el --- Completion-at-point settings  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(defcustom ceamx-completion-at-point-ui 'corfu
  "Choose `completion-at-point' UI between `corfu' or `lsp-bridge'."
  :group 'ceamx
  :type '(choice :tag "completion-at-point user interface"
           (const :tag "The `corfu' module" corfu)
           (const :tag "The `lsp-bridge' module" lsp-bridge)))

;; TODO: implement? should move to sleection
;; (defcustom ceamx-completion-ui 'vertico
;;   "Choose minibuffer completion UI between `mct' or `vertico'."
;;   :group 'ceamx
;;   :type '(choice :tag "Minibuffer user interface"
;;                  (const :tag "The `mct' module" mct)
;;                  (const :tag "The `vertico' module" vertico)))


(provide 'config-completion)
;;; config-completion.el ends here
