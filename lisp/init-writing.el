;;; init-writing.el --- Writing augmentation  -*- lexical-binding: t;  -*-

;; Copyright (c) 2024  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>
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


(autoload 'global-jinx-mode "jinx")
(autoload 'jinx-correct "jinx")
(autoload 'jinx-languages "jinx")

(add-hook 'ceamx-emacs-startup-hook #'global-jinx-mode)
(keymap-global-set "M-$" #'jinx-correct)
(keymap-global-set "C-M-$" #'jinx-languages)

(setopt jinx-languages "en")

(provide 'init-writing)
;;; init-writing.el ends here
