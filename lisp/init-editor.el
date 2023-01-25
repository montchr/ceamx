;;; init-editor.el --- Editing --- -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/ceamx
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

;;  Basic editor configuration

;;; Code:

(use-feature emacs
  :init
  (electric-indent-mode +1)
  (electric-pair-mode +1)
  (setq electric-pair-mode-preserve-balance nil)

  ;; mode-specific local-electric pairs
  ;; <https://www.lucacambiaghi.com/vanilla-emacs/readme.html#h:BE3F251D-5F39-4337-B27C-CFB81EE9A504>
  (defconst +default-electric-pairs electric-pair-pairs)
  (defun cmx/add-local-electric-pairs (pairs)
    "Example usage:
    (add-hook 'jupyter-org-interaction-mode '(lambda () (cmx/add-local-electric-pairs '())))
    "
    (setq-local electric-pair-pairs (append +default-electric-pairs pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))

  :hook
  ((org-mode . (lambda () (cmx/add-local-electric-pairs '((?= . ?=)
                                                     (?~ . ?~)))))))

(elpaca-use-package apheleia
  :init (apheleia-global-mode +1))

;;
;;; === PULSAR ======================================================================================
;;  > Pulse highlight line on demand or after running select functions
;;  <https://protesilaos.com/emacs/pulsar>

(elpaca-use-package pulsar
  :defer 5
  :config
  (setq pulsar-pulse           t)
  (setq pulsar-delay           0.055)
  (setq pulsar-iterations      10)
  (setq pulsar-face            'pulsar-magenta)
  (setq pulsar-highlight-face  'pulsar-yellow)
  (pulsar-global-mode 1))

(provide 'init-editor)
;;; init-editor.el ends here
