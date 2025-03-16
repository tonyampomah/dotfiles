{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;

  networking.hostName = "nixos";

  system.autoUpgrade.enable = true;
  system.autoUpgrade.dates = "weekly";

  # Automatic cleanup
  nix.gc.automatic = true;
  nix.gc.dates = "dailiy";
  nix.gc.options = "--delete-older-than 3d";
  nix.settings.auto-optimise-store = true;

  networking.networkmanager.enable = true;

  time.timeZone = "Europe/London";

  i18n.defaultLocale = "en_GB.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_GB.UTF-8";
    LC_IDENTIFICATION = "en_GB.UTF-8";
    LC_MEASUREMENT = "en_GB.UTF-8";
    LC_MONETARY = "en_GB.UTF-8";
    LC_NAME = "en_GB.UTF-8";
    LC_NUMERIC = "en_GB.UTF-8";
    LC_PAPER = "en_GB.UTF-8";
    LC_TELEPHONE = "en_GB.UTF-8";
    LC_TIME = "en_GB.UTF-8";
  };

  services.xserver.enable = true;

  services.xserver.displayManager.gdm.enable = true;
  services.xserver.displayManager.gdm.wayland = true;
  services.xserver.desktopManager.gnome.enable = true;

  programs.hyprland.enable = true;

  services.xserver.xkb = {
    layout = "gb";
    variant = "";
  };

  console.keyMap = "uk";

  services.printing.enable = true;

  # Enable sound with pipewire.
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  users.defaultUserShell=pkgs.zsh;

  programs.zsh.enable = true;

  users.users.tonyampomah.shell = pkgs.zsh; 

  programs = {
    zsh = {
      # enable = true;
      autosuggestions.enable = true;
      zsh-autoenv.enable = true;
      syntaxHighlighting.enable = true;
      ohMyZsh = {
        enable = true;
        theme = "robbyrussell";
        plugins = [
          "git"
          "npm"
          "history"
          "node"
          "rust"
          "deno"
        ];
      };
    };
  };

  users.users.tonyampomah = {
    isNormalUser = true;
    description = "Tony Ampomah";
    extraGroups = [ "networkmanager" "wheel" "docker" ];
    packages = with pkgs; [
    ];
  };

  programs.firefox.enable = true;

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    alacritty
    android-file-transfer
    ansible
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    audacity
    awscli2
    brave
    brightnessctl
    browserpass
    calibre
    cascadia-code
    catppuccin-cursors
    catppuccin-gtk
    cmake
    cmatrix
    cmus
    dbeaver-bin
    devenv
    fzf
    gcc
    git
    gnumake
    gnupg
    google-chrome
    grim
    htop
    hyprland
    hyprlock
    hyprpaper
    isync
    killall
    kitty
    languagetool
    ledger
    libgcc
    libnotify
    libtool
    libvterm
    libvterm-neovim
    lsof
    manrope
    mixxx
    mpc
    mpd
    mpv
    mu
    ncmpcpp
    neofetch
    networkmanagerapplet
    nextcloud-client
    nwg-look
    oh-my-zsh
    palenight-theme
    pandoc
    pass
    pavucontrol
    php
    pyenv
    python3
    ranger
    rhythmbox
    ripgrep
    rofi-calc
    rofi-emoji-wayland
    rofi-pass-wayland
    rofi-wayland
    slack
    slurp
    stow
    surfraw
    swaynotificationcenter
    swww
    texliveFull
    unzip
    vim
    vscode
    waybar
    waypaper
    wget
    whatsapp-for-linux
    whitesur-cursors
    whitesur-gtk-theme
    whitesur-icon-theme
    wirelesstools
    wl-clipboard
    wlogout
    yarn-berry
    zip
    zoom-us
    zsh
  ];

  fonts.packages = with pkgs; [
    noto-fonts
    jetbrains-mono
  ];

  system.stateVersion = "24.11";

  xdg.portal.enable =  true;

  xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-gtk ];

  virtualisation.docker.enable = true;

  virtualisation.waydroid.enable = true;

  environment.sessionVariables.NIXOS_OZONE_WL = "1";
}
