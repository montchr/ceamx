;;; init-ui-modeline-telephone-line.el --- Telephone Line modeline  -*- lexical-binding: t; -*-

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

;; <https://github.com/dbordak/telephone-line>

;;; Code:

(use-package telephone-line
  :config
  (after! [symex]
    (defface telephone-line-evil-symex
      '((t (:background "SlateBlue3" :inherit telephone-line-evil)))
      "Face used in evil color-coded segments when in Symex state."
      :group 'telephone-line-evil))

  (setq telephone-line-lhs
        '((evil . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil . (telephone-line-minor-mode-segment
                  telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((nil . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment) )
          (evil . (telephone-line-airline-position-segment))))
  (telephone-line-mode +1))

(provide 'init-ui-modeline-telephone-line)
;;; init-ui-modeline-telephone-line.el ends here
