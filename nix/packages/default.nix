{ self, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages = {
        aspell-with-dicts = (
          pkgs.aspellWithDicts (
            ds: with ds; [
              en
              en-computers
              en-science
            ]
          )
        );

        # via <https://github.com/nix-community/home-manager/blob/80546b220e95a575c66c213af1b09fe255299438/modules/services/emacs.nix#L186C1-L191C11>
        editor = pkgs.writeShellScriptBin "editor" ''
          exec emacsclient "''${@:---create-frame}"
        '';

        org-protocol-desktop-entry = pkgs.callPackage ./by-name/org-protocol-desktop-entry.nix {
          mimeTypes = [ self.lib.mimetypes.org-protocol ];
        };
      };
    };
}
