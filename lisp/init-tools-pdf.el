;;; init-tools-pdf.el --- Support for working with PDF files  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords: local, docs

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

;; <https://github.com/vedang/pdf-tools>

;;; Sources:

;; <https://github.com/jwiegley/dot-emacs/blob/master/init.org>

;; ~pdf-tools~ should be installed installed via Nixpkgs because it requires
;; some separate binaries.

;; TODO: <https://github.com/doomemacs/doomemacs/blob/master/modules/tools/pdf/config.el>

;;; Code:

(require 'ceamx-lib)

(use-feature! pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :custom
  (pdf-tools-handle-upgrades nil)
  :config
  (dolist
    (pkg
      '(pdf-annot pdf-cache pdf-dev pdf-history pdf-info pdf-isearch
         pdf-links pdf-misc pdf-occur pdf-outline pdf-sync
         pdf-util pdf-view pdf-virtual))
    (require pkg))
  (pdf-tools-install))

;; (use-package pdf-tools
;;   :commands (pdf-tools-install)

;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
;;   (add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))

;;   (setopt pdf-tools-handle-upgrades nil)
;;   :config
;;   (dolist
;;     (pkg
;;       '(pdf-annot pdf-cache pdf-dev pdf-history pdf-info pdf-isearch
;;          pdf-links pdf-misc pdf-occur pdf-outline pdf-sync
;;          pdf-util pdf-view pdf-virtual))
;;     (require pkg))
;;   (pdf-tools-install))

;; (use-package saveplace-pdf-view
;;   :defer 5)

(provide 'init-tools-pdf)
;;; init-tools-pdf.el ends here
