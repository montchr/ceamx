;;; ceamx-tools.el --- Ceamx Tools                   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: extensions, convenience

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

;;;; Requirements

(require 'hydra)
(require 'rect)

;;;; Variables

;;;; Customization

;;;; Functions

(defun ceamx-tools-pdf-annot-cleanup-windows-h ()
  "Kill leftover PDF annotation buffers with the document."
  (when (buffer-live-p pdf-annot-list-document-buffer)
    (pdf-info-close pdf-annot-list-document-buffer))
  (when (buffer-live-p pdf-annot-list-buffer)
    (kill-buffer pdf-annot-list-buffer))
  (let ((contents-buffer (get-buffer "*Contents*")))
    (when (and contents-buffer (buffer-live-p contents-buffer))
      (kill-buffer contents-buffer))))

;;;; Commands

;;;###autoload
(defun ceamx/ready-player-dired-preview-play-toggle ()
  "Call `ready-player-toggle-play-stop' on the currently-previewed media file."
  (interactive)
  (dired-preview-with-window
    (if-let ((file buffer-file-name)
              (media (concat "\\." (regexp-opt ready-player-supported-media t) "\\'"))
              (_ (string-match-p media file)))
      (call-interactively #'ready-player-toggle-play-stop)
      (user-error "Cannot do something useful with `ready-player' here"))))

;;;; Menus

;; source :: <https://github.com/abo-abo/hydra/wiki/Rectangle-Operations#rectangle-2>
(defhydra ceamx/rectangle-dispatch
  ( :body-pre (rectangle-mark-mode 1)
    :color pink
    :hint nil
    :post (deactivate-mark))
  "
  ^_k_^       _w_ copy      _o_pen       _N_umber-lines            |\\     -,,,--,,_
_h_   _l_     _y_ank        _t_ype       _e_xchange-point          /,`.-'`'   ..  \-;;,_
  ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark      |,4-  ) )_   .;.(  `'-'
^^^^          _u_ndo        _g_ quit     ^ ^                     '---''(./..)-'(_\_)
"
  ("k" rectangle-previous-line)
  ("j" rectangle-next-line)
  ("h" rectangle-backward-char)
  ("l" rectangle-forward-char)
  ("d" kill-rectangle)                    ;; C-x r k
  ("y" yank-rectangle)                    ;; C-x r y
  ("w" copy-rectangle-as-kill)            ;; C-x r M-w
  ("o" open-rectangle)                    ;; C-x r o
  ("t" string-rectangle)                  ;; C-x r t
  ("c" clear-rectangle)                   ;; C-x r c
  ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
  ("N" rectangle-number-lines)            ;; C-x r N
  ("r" (if (region-active-p)
         (deactivate-mark)
         (rectangle-mark-mode 1)))
  ("u" undo nil)
  ("g" nil))

(provide 'ceamx-tools)
;;; ceamx-tools.el ends here
