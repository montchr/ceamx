;;; init-vcs-magit.el --- Magit support              -*- lexical-binding: t; -*-

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

;;  <https://magit.vc/>

;;; Code:

;;; Requirements

(require 'lib-common)

;; FIXME: does not close transient when pressing ESC
;; FIXME: move to configuration for `transient'
(with-eval-after-load 'transient
  (defvar transient-map)
  (declare-function transient-quit-one "transient")

  ;; Always close transient with ESC
  (keymap-set transient-map "ESC" #'transient-quit-one))

;;; Configure Magit

(package! magit)

(with-eval-after-load 'magit
  (defvar magit-mode-map)
  (defvar magit-status-mode-map)

  (declare-function magit-discard "magit-apply")
  (declare-function magit-dispatch "magit")
  (declare-function magit-display-buffer-fullframe-status-v1 "magit-mode")
  (declare-function magit-file-dispatch "magit-files")
  (declare-function magit-restore-window-configuration "magit-mode")
  (declare-function magit-revert "magit-sequence")
  (declare-function magit-status "magit-status")

  (setopt magit-diff-refine-hunk t)     ; show granular diffs in selected hunk
  (setopt magit-save-repository-buffers nil) ; avoid side-effects (e.g. auto-format)
  ;; (setopt magit-revision-insert-related-refs nil) ; parent/related refs: rarely useful
  (setopt magit-process-finish-apply-ansi-colors t) ; render ANSI colors in process output

  (setopt magit-bury-buffer-function #'magit-restore-window-configuration)
  ;; <https://magit.vc/manual/magit/Switching-Buffers.html#index-magit_002ddisplay_002dbuffer_002dfullframe_002dstatus_002dv1>
  (setopt magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

  ;; These should be bound automatically when `magit-define-global-key-bindings'
  ;; is =default= (which is the default value), but that does not seem to work.
  (define-keymap :keymap (current-global-map)
    "C-x g"    #'magit-status
    "C-x M-g"  #'magit-dispatch
    "C-c M-g"  #'magit-file-dispatch)

  ;; TODO: why does this not have an effect?
  (define-keymap :keymap magit-status-mode-map
    "_" #'magit-revert
    "V" nil
    "x" #'magit-discard)

  (keymap-set magit-status-mode-map "_" #'magit-revert)
  ;; TODO: rebind?
  (keymap-set magit-status-mode-map "V" nil)
  ;; NOTE: Overrides default binding of `magit-reset-quickly'.
  (keymap-set magit-status-mode-map "x" #'magit-discard)

  (transient-append-suffix 'magit-commit "-n"
    '("-S" "Disable GPG signing" "--no-gpg"))
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash")))

(provide 'init-vcs-magit)
;;; init-vcs-magit.el ends here
