;; -*- lexical-binding: t; -*-

(require 'ceamx-lib)

(use-feature! zone
  :defer 30
  :autoload (zone-when-idle)
  :defines (zone-timer)
  :init
  (def-hook! ceamx-zone-when-idle-h ()
    'ceamx-emacs-startup-hook
    "Zone out when idle.
Return the new `zone' timer."
    (zone-when-idle (* 60 10))))

;; FIXME: broken: wrong type argument arrayp (for pgm arg)
;;        (where did this even come from? emacswiki?)
;; (defun zone-choose (pgm)
;;   "Choose a PGM to run for `zone'."
;;   (interactive
;;     (list
;;       (completing-read
;;         "Program: "
;;         (mapcar 'symbol-name zone-programs))))
;;   (let ((zone-programs (list (intern pgm))))
;;     (zone)))

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
