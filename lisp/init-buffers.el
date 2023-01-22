;;; init-buffers.el --- Buffer management -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/dotfield
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

;;; Commentary:

;;  Configuration for buffer management

;;; Code:

(use-feature emacs
  :init
  ;; Disable buffer line wrapping by default.
  ;; <https://www.emacswiki.org/emacs/TruncateLines>
  (set-default 'truncate-lines t))

(use-feature evil
  :general
  (+general-global-buffer
    "N" '(evil-buffer-new :which-key "new")))

(use-feature consult
  :general
  (+general-global-buffer
    "b"  '(consult-project-buffer :which-key "switch")
    "B"  '(consult-buffer :which-key "any")))

(elpaca-use-package dimmer
  :defer 5
  :after (which-key)

  :custom
  (dimmer-exclusion-regexp-list '("^\\*[h|H]elm.*\\*"
                                  "^\\*Minibuf-.*\\*"
                                  "^\\*Echo.*"
                                  "^.\\*which-key\\*$"))
  (dimmer-fraction 0.10)
  (dimmer-watch-frame-focus-events nil)

  :config
  (dimmer-mode 1)
  (dimmer-configure-which-key)
  (dimmer-configure-magit))

(provide 'init-buffers)
;;; init-buffers.el ends here
