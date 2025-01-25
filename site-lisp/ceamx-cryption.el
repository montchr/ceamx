;;; ceamx-cryption.el --- Ceamx: Cryptography        -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: extensions

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
;;; Code:

(require 'epa)

;; source :: <https://github.com/doomemacs/doomemacs/blob/2bc052425ca45a41532be0648ebd976d1bd2e6c1/modules/config/default/config.el#L45C1-L63C81>
(defun ceamx-cryption+epa-default-recipient ()
  "Infer a default encryption recipient for `epa-file-encrypt-to'.

Defaults to, in order:

    - The default value of `epa-file-encrypt-to'
    - The first enabled and non-expired key in the keyring
    - The value of `user-mail-address'"
  (or (default-value 'epa-file-encrypt-to)
      (unless (string-empty-p user-full-name)
        (when-let (context (ignore-errors (epg-make-context)))
          (cl-loop for key in (epg-list-keys context user-full-name 'public)
                   for subkey = (car (epg-key-sub-key-list key))
                   if (not (memq 'disabled (epg-sub-key-capability subkey)))
                   if (< (or (epg-sub-key-expiration-time subkey) 0)
                         (time-to-seconds))
                   collect (epg-sub-key-fingerprint subkey))))
      user-mail-address))

;; source :: <https://github.com/doomemacs/doomemacs/blob/2bc052425ca45a41532be0648ebd976d1bd2e6c1/modules/config/default/config.el#L45C1-L63C81>
(defun ceamx-cryption+epa-disable-key-prompt-a (&rest _)
  "Suppress `epa' prompts if `epa-file-encrypt-to' has a global default value.
File-local values for `epa-file-encrypt-to' will not be overwritten.

Intended as advice for `epa-file-write-region' (which see)."
  (unless (local-variable-p 'epa-file-encrypt-to)
    (setq-local epa-file-encrypt-to (default-value 'epa-file-encrypt-to))))

(provide 'ceamx-cryption)
;;; ceamx-cryption.el ends here
