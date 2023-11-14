;;; init-ui-modeline-nano.el ---N A N O Modeline initialization  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
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

;;

;;; Code:

(use-package nano-modeline
  ;; FIXME: :elpaca (nano-modeline :fetcher github :repo "rougier/nano-modeline")
  :after (nano-theme)

  :functions (nano-modeline-prog-mode
              nano-modeline-text-mode
              nano-modeline-org-mode
              nano-modeline-pdf-mode
              nano-modeline-mu4e-headers-mode
              nano-modeline-mu4e-message-mode
              nano-modeline-elfeed-entry-mode
              nano-modeline-term-mode
              nano-modeline-xwidget-mode
              nano-modeline-message-mode
              nano-modeline-org-capture-mode
              nano-modeline-org-agenda-mode)

  :config
  ;; (setopt nano-modeline-position #'nano-modeline-footer)
  (when (eq nano-modeline-position #'nano-modeline-header)
    (setq-default mode-line-format (list "%_"))
    (setq mode-line-format (list "%_")))

  (add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
  (add-hook 'text-mode-hook            #'nano-modeline-text-mode)
  (add-hook 'org-mode-hook             #'nano-modeline-org-mode)
  (add-hook 'pdf-view-mode-hook        #'nano-modeline-pdf-mode)
  (add-hook 'mu4e-headers-mode-hook    #'nano-modeline-mu4e-headers-mode)
  (add-hook 'mu4e-view-mode-hook       #'nano-modeline-mu4e-message-mode)
  (add-hook 'elfeed-show-mode-hook     #'nano-modeline-elfeed-entry-mode)
  (add-hook 'elfeed-search-mode-hook   #'nano-modeline-elfeed-search-mode)
  (add-hook 'term-mode-hook            #'nano-modeline-term-mode)
  (add-hook 'xwidget-webkit-mode-hook  #'nano-modeline-xwidget-mode)
  (add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
  (add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
  (add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode)

  ;; The default modeline mode.
  (nano-modeline-text-mode t))

(provide 'init-ui-modeline-nano)
;;; init-ui-modeline-nano.el ends here
