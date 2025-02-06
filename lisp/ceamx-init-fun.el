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

(provide 'ceamx-init-fun)
;;; ceamx-init-fun.el ends here
