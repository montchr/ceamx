;;;; templates/emacs-lisp-mode.eld

emacs-lisp-mode

(pkg ";;" n
     ";;; `" (s name) "' :: <" (s url) ">" n
     ";;" n
     n
     "(use-package " name n>
     ":config" n>
     ")")
(sset "(setq " (s name) " " (s val) ")")
