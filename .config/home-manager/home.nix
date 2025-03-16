{ config, pkgs, ... }:

{
  home.username = "tonyampomah";
  home.homeDirectory = "/home/tonyampomah";

  home.stateVersion = "24.11";

  home.packages = [
    pkgs.appimage-run
    pkgs.nodejs_22
    pkgs.nodePackages.prettier

    (pkgs.nerdfonts.override { fonts = [
                                 "FantasqueSansMono"
                                 "FiraCode"
                                 "DroidSansMono"
                               ]; })
  ];

  home.file = {
  };

  home.sessionVariables = {
    EDITOR = "emacs";
  };

  programs.home-manager.enable = true;

  programs.mu.enable = true;

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    extraPackages = epkgs: [
      epkgs.mu4e
    ];
  };

}
