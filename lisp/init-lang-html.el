;;; init-lang-html.el ---HTML and templating engine support  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chmont@proton.me>
;; Keywords:

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

;; Configuration for ~web-mode~ and related.

;; <https://web-mode.org/>

;;; Code:

(use-package web-mode
  :commands (web-mode)

  :init
  ;; TODO: refactor
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

  :config
  (setopt web-mode-engines-alist
          '(("php" . "\\.phtml\\'")
            ("blade" . "\\.blade\\.")))

  ;; TODO: revisit this...
  ;; NOTE: This method of setting customizations is unusual,
  ;;       but recommended by `web-mode' documentation.
  (defun +web-mode--customization-hook ()
    "Customization hook for `web-mode'"
    (setopt web-mode-markup-indent-offset 2)
    (setopt web-mode-css-indent-offset 2)
    (setopt web-mode-code-indent-offset 2)
    (setopt web-mode-enable-auto-pairing t)
    (setopt web-mode-enable-css-colorization t)
    (setopt web-mode-enable-block-face t)
    (setopt web-mode-enable-part-face t))
  (add-hook 'web-mode-hook '+web-mode--customization-hook)

  ;; NOTE: The following customizations should NOT be set
  ;;       in `+web-mode--customization-hook'.
  (setopt web-mode-enable-current-element-highlight t))

(provide 'init-lang-html)
;;; init-lang-html.el ends here
