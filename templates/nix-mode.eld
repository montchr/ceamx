;;; templates/nix-mode.eld

nix-mode

(letin "let" n> r> n "in ")
;; FIXME: indentation wrong
(comm "/*" n> > r> " :: " p " -> " p n> > "*/")
