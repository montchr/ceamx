{ config, pkgs, lib, ... }:
let
  cfg = config.programs.emacs.ceamx;
  isEnabled = config.programs.emacs.enable && cfg.enable;
in {
  options.programs.emacs.ceamx = {
    enable = lib.mkEnableOption "Ceamx Emacs configuration";
  };

  config = lib.mkIf isEnabled {
    programs.emacs.extraPackages = epkgs:
      with epkgs; [
        treesit-grammars.with-all-grammars
        treesit-auto
      ];

    home.packages = [
      pkgs.fd
      pkgs.ripgrep
    ];
  };
}
