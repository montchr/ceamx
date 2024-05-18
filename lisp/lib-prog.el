;;; lib-prog.el --- Assorted helper callables for programming modes  -*- lexical-binding: t;  -*-

;; Copyright (c) 2024  Chris Montgomery <chmont@proton.me>

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

(defun ceamx-lsp-client-set-priority (client priority)
  "Change the PRIORITY of an LSP-Mode CLIENT."
  (require 'lsp-mode)
  (if-let (client (gethash client lsp-clients))
      (setf (lsp--client-priority client)
            priority)
    (error "No LSP client named %S" client)))

(provide 'lib-prog)
;;; lib-prog.el ends here
