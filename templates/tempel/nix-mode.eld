nix-mode nix-ts-mode

;; FIXME: indentation (in both modes)

(attrset & p " = {" n> p " = " q ";" n "};" %)
(imports & "imports = [" n> r n "];" %)
(lines "''" n> r> n> "''")
(mkopt "mkOption {" n> "type = with types; " p ";" n> "default = " p ";" n> "description = ''" n> p n "'';" n "};")
(mkoptenable "enable = lib.mkEnableOption \"" p "\"; " %)

(inhr "inherit (" p ") " p ";")
(ihostp "inherit (pkgs.stdenv.hostPlatform) " p ";" q)

(letin "let" n> r> q n "in ")
(comm "/*" n> > r> " :: " p " -> " p n> > "*/")

(modargs "{ " (r "config, lib, pkgs") ", ... }:" o)

(modbasic (i modargs) n "{" n> q n "}")

(persys & "\
  perSystem =
    { " (p "inputs', ") (p "pkgs")", ... }:
    {" n> > r> n> "};" %)

(withsys & "withSystem " (p "system") " ({ " (p "inputs', config, ") "pkgs, ... }: {" n r> n "})")
