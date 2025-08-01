{ inputs, ... }:
{
  perSystem =
    { config, pkgs, ... }:
    {
      devshells.default =
        let
          cats = inputs.apparat.lib.devshell.categorised [
            "formatters"
            "tools"
          ];
          formatter = cats.formatters.pkg;
          tool = cats.tools.pkg;
        in
        {
          devshell.name = "Ceamx";
          commands = [
            # TODO: remove this temporary stuff for mail testing
            (tool pkgs.pizauth)
            (tool pkgs.msmtp)
            (tool pkgs.isync)

            (tool pkgs.just)
            (formatter config.formatter)
          ];
          env = [
            {
              name = "NIXFMT_MAX_WIDTH";
              value = 80;
            }
          ];
          devshell.packages = [ pkgs.fd ];
        };
    };
}
