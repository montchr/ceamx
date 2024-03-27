nix-mode nix-ts-mode

(inhr "inherit (" p ") " p ";")
(ihostp "inherit (pkgs.stdenv.hostPlatform) " p ";" q)

(letin "let" n> r> q n "in ")
;; FIXME: indentation wrong
(comm "/*" n> > r> " :: " p " -> " p n> > "*/")

(modargs "{ " (r "config, lib, pkgs") ", ... }:" o)
