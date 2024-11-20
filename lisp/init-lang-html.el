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

(package! web-mode
  ;; TODO: refactor
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

  (setopt web-mode-engines-alist
          '(("php" . "\\.phtml\\'")
            ("blade" . "\\.blade\\.")))

  ;; Defer to `electric-pair-mode' or similar.
  (setopt web-mode-enable-auto-pairing nil)

  (setopt web-mode-enable-css-colorization t
          web-mode-enable-block-face t
          web-mode-enable-part-face t
          web-mode-enable-current-element-highlight t))

;;; emmet-mode

;; - website :: <https://github.com/smihica/emmet-mode>
;; - reference ::
;; - <https://github.com/smihica/emmet-mode/blob/master/README.md#usage>

;; NOTE: This package is unmaintained!

(package! emmet-mode
  (setopt emmet-move-cursor-between-quotes t)

  (add-hook 'css-mode-hook #'emmet-mode)
  (after! web-mode
    (add-hook 'web-mode-hook #'emmet-mode)))

(provide 'init-lang-html)
;;; init-lang-html.el ends here
