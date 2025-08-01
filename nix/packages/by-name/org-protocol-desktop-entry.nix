{
  emacs,
  makeDesktopItem,

  mimeTypes ? [ ],
}:

(makeDesktopItem (finalAttrs: {
  inherit mimeTypes;
  pname = "Org-Protocol";
  desktopName = finalAttrs.pname;
  keywords = [
    "Org-Mode"
    "Emacs"
  ];
  comment = "Intercept calls from emacsclient to trigger custom actions";
  startupWMClass = "Emacs";
  exec = "${emacs}/bin/emacsclient -- %u";
  terminal = false;
  icon = "emacs";
  categories = [ "Office" ];
}))
