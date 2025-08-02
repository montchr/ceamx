;; -*- lexical-binding: t; -*-

(require 'ceamx-lib)

(package! wordel
  (after! wordel
    ;; `puni-mode' binds keys that conflict with `wordel'.  There's also
    ;; no need for it in a game.
    (add-hook 'wordel-mode-hook #'puni-disable-puni-mode)

    (define-keymap :keymap wordel-mode-map
      "C-a" #'wordel-first-column
      "C-b" #'wordel-prev-column
      "C-e" #'wordel-last-column
      "C-f" #'wordel-next-column)))

(package! (dnd5e-api :host github :repo "VivianWilde/emacs-dnd5e-api"))

(provide 'ceamx-init-fun)
;;; ceamx-init-fun.el ends here
