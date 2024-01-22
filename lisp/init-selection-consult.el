;;; init-selection-consult.el --- Configuration for Consult  -*- lexical-binding: t; -*-

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

;; "Consulting completing-read"
;;
;; <https://github.com/minad/consult>

;; <https://www.gnu.org/software/emacs/manual/html_node/elisp/Minibuffer-Completion.html>

;; `fd' is intentionally excluded from `consult' core.
;; however, support can be added manually: <https://github.com/minad/consult/wiki#find-files-using-fd>

;;; Code:

(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  (setopt register-preview-delay 0.5)
  (setopt register-preview-function #'consult-register-format)
  (setopt xref-show-definitions-function #'consult-xref)
  (setopt xref-show-xrefs-function #'consult-xref)
  (advice-add #'register-preview :override #'consult-register-window)

  :config
	(define-keymap :keymap global-map
    "<remap> <bookmark-jump>"                   #'consult-bookmark
    "<remap> <repeat-complex-command>"          #'consult-complex-command
    "<remap> <goto-line>"                       #'consult-goto-line
    "<remap> <imenu>"                           #'consult-imenu
    "<remap> <isearch-edit-string>"             #'consult-isearch-history
    "<remap> <locate>"                          #'consult-locate
    "<remap> <load-theme>"                      #'consult-theme
    "<remap> <man>"                             #'consult-man
    "<remap> <recentf-open-files>"              #'consult-recent-file
    "<remap> <switch-to-buffer>"                #'consult-buffer
    "<remap> <switch-to-buffer-other-window>"   #'consult-buffer-other-window
    "<remap> <switch-to-buffer-other-frame>"    #'consult-buffer-other-frame
    "<remap> <yank-pop>"                        #'consult-yank-pop
    "<remap> <project-switch-to-buffer>"        #'consult-project-buffer

    ;; C-c bindings (mode-specific-map)
    ;; TODO: verify
    "C-c s h"  #'consult-history
    "C-c s m"  #'consult-mode-command
    "C-c s k"  #'consult-kmacro

    ;; Custom M-# bindings for fast register access
    "M-#"    #'consult-register-load
    "M-'"    #'consult-register-store
    "C-M-#"  #'consult-register

    ;; M-g bindings (goto-map)
    "M-g e"  #'consult-compile-error
    "M-g f"  #'consult-flymake               ;; Alternative: consult-flycheck
    "M-g o"  #'consult-outline               ;; Alternative: consult-org-heading
    "M-g m"  #'consult-mark
    "M-g k"  #'consult-global-mark
    "M-g i"  #'consult-imenu
    "M-g I"  #'consult-imenu-multi


    ;; M-s bindings (search-map)
    "M-s d"  #'consult-find
    "M-s D"  #'consult-locate
    "M-s g"  #'consult-ripgrep
    "M-s G"  #'consult-git-grep
    "M-s l"  #'consult-line
    "M-s L"  #'consult-line-multi
    "M-s k"  #'consult-keep-lines
    "M-s u"  #'consult-focus-lines)

  ;;; `isearch' integration:
  (keymap-global-set "M-s e" #'consult-isearch-history)
  (keymap-set minibuffer-local-map "M-s" #'consult-history) ; orig. next-matching-history-element
  (keymap-set minibuffer-local-map "M-r" #'consult-history) ; orig. previous-matching-history-element
  ;; Needed by consult-line to detect isearch.
  (keymap-set isearch-mode-map "M-s l" #'consult-line)
  (keymap-set isearch-mode-map "M-s L" #'consult-line-multi)

  (setopt consult-preview-key 'any)

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setopt consult-narrow-key "<") ;; (kbd "C-+")

  ;; Make narrowing help available in the minibuffer.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'embark-prefix-help-command)

  ;;; Use Orderless as pattern compiler for consult-grep/ripgrep/find.
  ;;; via <https://github.com/minad/consult/wiki#use-orderless-as-pattern-compiler-for-consult-grepripgrepfind>
  ;;; FIXME: both options break!

  ;; (defun +consult--orderless-regexp-compiler (input type &rest _config)
  ;;   (setq input (orderless-pattern-compiler input))
  ;;   (cons
  ;;    (mapcar (lambda (r) (consult--convert-regexp r type)) input)
  ;;    (lambda (str) (orderless--highlight input str))))

  ;; OPTION 1: Activate globally for all consult-grep/ripgrep/find/...
  ;; (setopt consult--regexp-compiler #'consult--orderless-regexp-compiler)

  ;; OPTION 2: Activate only for some commands, e.g., consult-ripgrep!
  ;; (defun +consult--with-orderless (&rest args)
  ;;   (minibuffer-with-setup-hook
  ;;       (lambda ()
  ;;         (setq-local consult--regexp-compiler #'+consult--orderless-regexp-compiler))
  ;;     (apply args)))
  ;; (advice-add #'consult-ripgrep :around #'+consult--with-orderless)

  ;;; --- buffers ---

  ;;; Pre-select nearest heading for `consult-org-heading' and `consult-outline'
  ;;; <https://github.com/minad/consult/wiki#pre-select-nearest-heading-for-consult-org-heading-and-consult-outline-using-vertico>

  (defvar +consult--previous-point nil
    "Location of point before entering minibuffer.
  Used to preselect nearest headings and imenu items.")

  (defun +consult--set-previous-point ()
    "Save location of point. Used before entering the minibuffer."
    (setq +consult--previous-point (point)))

  (defun +consult-vertico--update-choose (&rest _)
    "Pick the nearest candidate rather than the first after updating candidates."
    (when (and +consult--previous-point
               (memq current-minibuffer-command
                     '(consult-org-heading consult-outline)))
      (setq vertico--index
            (max 0 ; if none above, choose the first below
                 (1- (or (seq-position
                          vertico--candidates
                          +consult--previous-point
                          (lambda (cand point-pos) ; counts on candidate list being sorted
                            (> (cl-case current-minibuffer-command
                                 (consult-outline
                                  (car (consult--get-location cand)))
                                 (consult-org-heading
                                  (get-text-property 0 'consult--candidate cand)))
                               point-pos)))
                         (length vertico--candidates))))))
    (setq +consult--previous-point nil))

  (advice-add #'consult-org-heading :before #'+consult--set-previous-point)
  (advice-add #'consult-outline :before #'+consult--set-previous-point)
  (advice-add #'vertico--update :after #'+consult-vertico--update-choose)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  (with-eval-after-load 'projectile
    (autoload 'projectile-project-root "projectile")
    (setopt consult-project-function (lambda (_) (projectile-project-root)))))


;; <https://github.com/minad/consult#embark-integration>
(use-package embark-consult
  :after (embark consult)
  :init
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))


(provide 'init-selection-consult)
;;; init-selection-consult.el ends here
