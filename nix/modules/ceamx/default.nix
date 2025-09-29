{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.programs.emacs.ceamx;
  isEnabled = config.programs.emacs.enable && cfg.enable;

  inherit (pkgs) enchant;
in
{
  options.programs.emacs.ceamx = {
    enable = lib.mkEnableOption "Ceamx Emacs configuration";
  };

  config = lib.mkIf isEnabled {
    programs.emacs.extraPackages = epkgs: [
      (epkgs.jinx.override { enchant2 = enchant; })
      epkgs.pdf-tools
      # XXX(2025-09-02): build failure
      #      epkgs.ready-player
      epkgs.treesit-grammars.with-all-grammars
      epkgs.treesit-auto
    ];

    home.packages =
      let
        languageServerPkgs =
          [
            pkgs.bash-language-server
            pkgs.editorconfig-core-c
            pkgs.systemd-lsp
            pkgs.taplo-lsp # toml language server
          ]
          ++ (with pkgs.nodePackages; [
            dockerfile-language-server-nodejs
            typescript-language-server
            vscode-langservers-extracted
            yaml-language-server
          ]);

        writingHelpers = [
          enchant
          pkgs.languagetool
        ];
      in
      [
        ### Emacs Helpers

        pkgs.emacs-lsp-booster

        ### Common Utilities

        pkgs.fd
        pkgs.imagemagick # for image-dired
        (pkgs.ripgrep.override { withPCRE2 = true; })
        pkgs.zstd # for compression in supported contexts

      ]
      ++ languageServerPkgs
      ++ writingHelpers;
  };
}
