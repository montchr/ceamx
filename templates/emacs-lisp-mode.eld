;;;; templates/emacs-lisp-mode.eld

emacs-lisp-mode

(kb "(keymap-set " (s map) " \"" (s key) "\" #'" (s cmd) ")" q)
(kbg "(keymap-global-set \"" (s key) "\" #'" (s cmd) q ")" q)
(kbs "(define-keymap :keymap " (p "(current-global-map)" map) n>
        "\"" p "\" #'" p ")" q)
(weal "(with-eval-after-load '" (s feature) n>
         r ")")

(pkg ";;" n
     ";;; `" (s name) "' :: <" (s url) ">" n
     ";;" n
     n
     "(use-package " name n>
     ":config" n>
     ")")
(seto "(setopt " p " " p ")")
(sset "(setq " (s name) " " (s val) ")")
