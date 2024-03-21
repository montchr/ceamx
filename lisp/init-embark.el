;;; init-embark.el --- Configuration for Embark  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>

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

;; Install and pre-configure the Embark package


;; [[file:../config.org::*Install and pre-configure the Embark package][Install and pre-configure the Embark package:1]]
(require 'lib-common)

(package! embark
  (setopt prefix-help-command #'embark-prefix-help-command)

  (keymap-global-set "C-;" #'embark-act)
  (keymap-global-set "M-." #'embark-dwim)

  (defer! 2
    (require 'embark)))
;; Install and pre-configure the Embark package:1 ends here

;; Configure =display-buffer-alist= rules for Embark windows


;; [[file:../config.org::*Configure =display-buffer-alist= rules for Embark windows][Configure =display-buffer-alist= rules for Embark windows:1]]
(with-eval-after-load 'embark
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
;; Configure =display-buffer-alist= rules for Embark windows:1 ends here

(provide 'init-embark)
;;; init-embark.el ends here
