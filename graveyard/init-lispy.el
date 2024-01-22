


;;; `lispy' :: <https://github.com/abo-abo/lispy>
(use-package lispy
  :commands (lispy-mode)

  :init
  (add-hook 'ceamx-lisp-init-hook #'lispy-mode)

  :config
  ;; Prevent `lispy' from inserting escaped quotes when already inside a string,
  ;; in favor of just moving past the closing quote as I would expect.
  (setopt lispy-close-quotes-at-end-p t)

  ;; TODO: Remove after <https://github.com/abo-abo/lispy/pull/619> (if ever?)
  ;; TODO: `keymap-unset' does not work here (with either nil or t) -- why not?
  ;; (keymap-unset lispy-mode-map "`" t)
  (keymap-set lispy-mode-map "`"  #'self-insert-command)

  (keymap-set lispy-mode-map "M-v" nil)

  (after! 'popper
    (push "\\*lispy-message\\*" 'popper-reference-buffers)))

;; `lispyville' :: <https://github.com/noctuid/lispyville>
(use-package lispyville
  :disabled t
  :after (evil lispy)
  :defines (lispyville-key-theme)

  :init
  ;; Enable `lispyville' when `lispy' is enabled.
  (add-hook 'lispy-mode-hook #'lispyville-mode)

  ;; NOTE: `setopt' throws warning on mismatched type
  (setq lispyville-key-theme
    '((operators normal)
       c-w
       (prettify insert)
       (atom-movement t)
       slurp/barf-lispy
       additional
       additional-insert))


  :config
  (lispyville-set-key-theme)

  (add-hook! 'evil-escape-inhibit-functions
    (defun +lispy-inhibit-evil-escape-fn ()
      (and lispy-mode (evil-insert-state-p)))))

(after! 'lispy
  (when (boundp 'lispy-outline)
    (setq lispy-outline
      (concat
        ;; `lispy-mode' requires `lispy-outline' start with ^
        (unless (string-prefix-p "^" +emacs-lisp-outline-regexp) "^")
        +emacs-lisp-outline-regexp))))
