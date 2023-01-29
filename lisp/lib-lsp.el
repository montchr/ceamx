;;; lib-lsp.el --- LSP library functions -*- lexical-binding: t -*-

;; Copyright (c) 2014-2022  Henrik Lissner
;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>
;; SPDX-License-Identifier: GPL-3.0-or-later OR MIT

;; Author: Henrik Lissner
;;         Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Modified: 29 January, 2023
;; Created: 29 January, 2023
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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

;;  Helpers for LSP server configurations, including `lsp-mode' and `eglot'.
;;
;;  Portions of this file are derived from Doom Emacs.

;;; Code:

;; <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/modules/tools/lsp/%2Blsp.el#L90-L114>
(defun +lsp-defer-server-shutdown-a (fn &optional restart)
  "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process). It also prevents the
server getting expensively restarted when reverting buffers."
  (if (or lsp-keep-workspace-alive
          restart
          (null +lsp-defer-shutdown)
          (= +lsp-defer-shutdown 0))
      (funcall fn restart)
    (when (timerp +lsp--deferred-shutdown-timer)
      (cancel-timer +lsp--deferred-shutdown-timer))
    (setq +lsp--deferred-shutdown-timer
          (run-at-time
           (if (numberp +lsp-defer-shutdown) +lsp-defer-shutdown 3)
           nil (lambda (workspace)
                 (with-lsp-workspace
                  workspace
                  (unless (lsp--workspace-buffers workspace)
                    (let ((lsp-restart 'ignore)) (funcall fn)))))
           lsp--cur-workspace))))


(provide 'lib-lsp)
;;; lib-lsp.el ends here
