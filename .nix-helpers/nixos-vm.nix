
# This file is used to build a NixOS VM that can be used to test Termonad in a
# known-working desktop environment (DE) / window manager (WM).
#
# This is helpful for developers using a minimal WM (like XMonad), but want to
# check how Termonad looks in a more full-featured desktop environment.
#
# You can build the VM with the following command:
#
# $ nix-build "$(nix-instantiate --eval -E 'with import ./nixpkgs.nix {}; path')"/nixos -A vm -I nixpkgs=./nixpkgs.nix -I nixos-config=./nixos-vm.nix
#
# You can run the resulting VM with the following command:
#
# $ ./result/bin/run-termonad-nixos-vm-vm -m 4G
#
# The `-m` flag allows you to specify how much RAM is used.

{ config, pkgs, lib, ... }:

{
  nixpkgs.overlays = import ./overlays.nix;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "termonad-nixos-vm";
  networking.networkmanager.enable = true;
  networking.useDHCP = lib.mkDefault true;

  time.timeZone = "UTC";

  environment.systemPackages = with pkgs; [
    curl
    file
    firefox
    htop
    jq
    ltrace
    nix-bash-completions
    pkgconfig
    psmisc
    screen
    strace
    termonad
    tree
    unzip
    vimHugeX
    wget
    xterm
  ];

  environment.variables = {
    EDITOR = "vim";
  };

  fonts.fonts = with pkgs; [
    dejavu_fonts ipafont
  ];

  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [ "C.UTF-8/UTF-8" "en_US.UTF-8/UTF-8" "ja_JP.UTF-8/UTF-8" ];
    inputMethod = {
      enabled = "fcitx5";
      fcitx5.addons = with pkgs; [ fcitx5-mozc fcitx5-rime fcitx5-chinese-addons ];
    };
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  programs.bash.enableCompletion = true;

  services.xserver = {
    enable = true;
    layout = "us";
    desktopManager.gnome.enable = true;
  };

  services.openssh = {
    enable = true;
    settings = {
      X11Forwarding = true;
      KbdInteractiveAuthentication = true;
      ChallengeResponseAuthentication = true;
      PasswordAuthentication = true;
      PermitRootLogin = "yes";
      MaxAuthTries = 12;
    };
  };

  security.sudo = {
    enable = true;
    extraConfig = ''
      %wheel      ALL=(ALL:ALL) NOPASSWD: ${pkgs.systemd}/bin/poweroff
      %wheel      ALL=(ALL:ALL) NOPASSWD: ${pkgs.systemd}/bin/reboot
      %wheel      ALL=(ALL:ALL) NOPASSWD: ${pkgs.systemd}/bin/systemctl suspend
    '';
  };

  users.extraUsers.myuser = {
    extraGroups = [ "audio" "systemd-journal" "video" "wheel" ];
    initialPassword = "foobar";
    isNormalUser = true;
  };

  system.stateVersion = "23.05"; # Did you read the comment?
}
