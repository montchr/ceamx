;;; lib-keybinds.el --- Keybinding helpers -*- lexical-binding: t -*-

;; Copyright (c) 2023 Chris Montgomery <chris@cdom.io>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author:  Chris Montgomery <chris@cdom.io>
;; URL:  https://git.sr.ht/~montchr/ceamx
;; Created:  06 July 2023
;; Version:  0.1.0

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

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;;; Commentary:

;;  Helper functions and macros for working with keybindings.

;;; Code:

;; via <https://github.com/noctuid/dotfiles/blob/434ddb77c4b40f4b7ab2246cc2254aa4f408b16f/emacs/.emacs.d/awaken.org>
(defmacro cmx/with-q-to-exit (&rest body)
  `(progn ,@body
          (general-def 'normal 'local "q" #'delete-frame)))

;; via <https://github.com/noctuid/dotfiles/blob/434ddb77c4b40f4b7ab2246cc2254aa4f408b16f/emacs/.emacs.d/awaken.org>
(defun cmx/with-q-to-quit ()
  "Bind q to `quit-window' in the current buffer."
  (general-def 'normal 'local "q" #'quit-window))

(provide 'lib-keybinds)
;;; lib-keybinds.el ends here
