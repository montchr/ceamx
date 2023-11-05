nix-mode

(modprog "\
{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.programs." (p "pname" pname) ";
in {
  options.programs." (s pname) " = {
    enable = lib.mkEnableOption \"" (s pname) "\";

    package = lib.mkPackageOption pkgs \"" (s pname) "\" {};
  };

  config = lib.mkIf cfg.enable {
    home.packages = lib.singleton cfg.package;
    " q "
  };
}
")
