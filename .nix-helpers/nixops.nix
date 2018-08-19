
{
  network.description = "Gnome With Termonad";

  termonad-machine =
    { config, pkgs, ...}:
    {
      imports = [ ];

      deployment = {
        targetEnv = "virtualbox";
        virtualbox = {
          headless = false;
          memorySize = 2024;
          vcpu = 1;
        };
      };

      environment = {
        systemPackages =
          let
            pkgList = with pkgs; [
              acpi aspell aspellDicts.en autojump bash bash-completion bc
              chromium curl dmenu emacs evince file firefoxWrapper gcc geeqie
              gimp gitAndTools.gitFull gitAndTools.hub gnumake gnupg hexchat
              htop jq k2pdfopt ltrace manpages ncurses nix-bash-completions
              nixops p7zip pkgconfig psmisc python3 redshift roxterm screen
              strace tree unzip usbutils vimHugeX wget wirelesstools
              xfce.terminal xorg.xbacklight xorg.xmodmap xscreensaver xterm
              zlib
            ];
            termonad = import ../default.nix { };
          in [ termonad ] ++ pkgList;
        variables.EDITOR = "vim";
      };

      fonts.fonts = with pkgs; [
        dejavu_fonts ipafont source-code-pro ttf_bitstream_vera
      ];

      i18n = {
        consoleFont = "Lat2-Terminus16";
        consoleKeyMap = "us";
        defaultLocale = "en_US.UTF-8";
        inputMethod = {
          enabled = "fcitx";
          fcitx.engines = with pkgs.fcitx-engines; [ mozc ];
        };
      };

      programs.bash.enableCompletion = true;

      services = {
        xserver = {
          enable = true;
          layout = "us";
          desktopManager.gnome3.enable = true;
        };
        openssh = {
          enable = true;
          forwardX11 = true;
          challengeResponseAuthentication = true;
          passwordAuthentication = true;
          permitRootLogin = "yes";
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
    };
}
