;;; init-eglot.el --- Eglot support                  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

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

;; Configure Eglot.

;; TODO: Consider implementing Doom's approach to handling performance.
;; <https://github.com/doomemacs/doomemacs/blob/03d692f129633e3bf0bd100d91b3ebf3f77db6d1/modules/tools/lsp/config.el>

;;; Code:

(require 'lib-common)

(use-package eglot
  :commands (eglot eglot-ensure)

  :init
  (def-advice! +eglot--ensure-available-mode (fn)
    :around #'eglot-ensure
    "Run `eglot-ensure' in supported modes."
    (when (alist-get major-mode eglot-server-programs nil nil
            (lambda (modes key)
              (if (listp modes)
                (member key modes)
                (eq key modes))))
      (funcall fn)))

  (setopt eglot-sync-connect 1)
  (setopt eglot-autoshutdown t)
  (setopt eglot-send-changes-idle-time 0.5)

  ;; Disable events buffer, which poses performance issues over time as the
  ;; buffer grows in a longer-running Emacs instance.
  (setopt eglot-events-buffer-size 0)

  ;; Prevent frequent focus-stealing.
  (setopt eglot-auto-display-help-buffer nil)

  :config
  ;; Register eglot help buffers as popup windows.
  (after! [popper]
    (defvar popper-reference-buffers)
    ;; TODO: make this a macro?
    (setopt popper-reference-buffers
      (append popper-reference-buffers '("^\\*eglot-help"))))

  ;; TODO: <https://github.com/doomemacs/doomemacs/blob/master/modules/tools/lsp/%2Beglot.el>
;; (def-advice! +lsp--defer-server-shutdown-a (fn &optional server)
;;   :around #'eglot--managed-mode
;;   "Defer server shutdown for a few seconds.
;; This gives the user a chance to open other project files before the server is
;; auto-killed (which is a potentially expensive process). It also prevents the
;; server getting expensively restarted when reverting buffers."
;;   (letf! (defun eglot-shutdown (server)
;;            (if (or (null +lsp-defer-shutdown)
;;                    (eq +lsp-defer-shutdown 0))
;;                (prog1 (funcall eglot-shutdown server)
;;                  (+lsp-optimization-mode -1))
;;              (run-at-time
;;               (if (numberp +lsp-defer-shutdown) +lsp-defer-shutdown 3)
;;               nil (lambda (server)
;;                     (unless (eglot--managed-buffers server)
;;                       (prog1 (funcall eglot-shutdown server)
;;                         (+lsp-optimization-mode -1))))
;;               server)))
;;          (funcall fn server)))


  )

;; FIXME: install package
;; (use-package flycheck-eglot
;;   :after (flycheck eglot)
;;   :init
;;   (add-hook 'eglot-managed-mode-hook #'flycheck-eglot-mode))

;; FIXME: install package
;;; `consult-eglot' :: <https://github.com/mohkale/consult-eglot>
;; (use-package consult-eglot
;;   :after (consult eglot))

(provide 'init-eglot)
;;; init-eglot.el ends here
