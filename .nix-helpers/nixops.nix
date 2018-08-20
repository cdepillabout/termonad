# This file can be used with `nixops` to create a virtual machine that has
# Termonad installed.
#
# I use this to test out Termonad in a desktop environment with a menubar.
#
# On my development machine, I use XMonad as a Window Manager, so there are
# no Window decorations for any X application.  This file creates a VM
# with Termonad installed in Gnome 3.  This lets you see what Termonad
# looks like when it has Window decorations, a title bar, etc.
#
# A virtual machine can be created based on this file with the following
# commands:
#
# $ nixops create --deployment termonad-test .nix-helpers/nixops.nix
# $ nixops deploy --deployment termonad-test
#
# This should open up a VirtualBox machine and start installing Gnome 3,
# Termonad, etc.
#
# You should be able to login with the username "myuser" and password "foobar".
#
# When you are done you can destroy the machine and delete the deployment:
#
# $ nixops destroy --deployment termonad-test
# $ nixops delete --deployment termonad-test
#

{
  network.description = "Gnome With Termonad";

  termonad-machine =
    { config, pkgs, ...}:
    {
      imports = [ ];

      deployment = {
        targetEnv = "virtualbox";
        virtualbox = {
          disks.disk1.size = 20480;
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
