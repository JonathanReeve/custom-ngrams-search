{ pkgs, config, ... }: {
  imports = [
    ./hardware-configuration.nix
  ];
  environment.systemPackages = with pkgs; [
    vim git unzip ripgrep tree bat fd bottom fish tmux wget
  ];
  time.timeZone = "America/New_York";
  boot.cleanTmpDir = true;
  networking.hostName = "custom-ngrams-search";
  networking.firewall.allowPing = true;
  services.openssh.enable = true;
  swapDevices = [{ device = "/swapfile"; size = 8192; } ];
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDqm3Uw8BKvlpTxvcPFUYt3uQ8V72odts2hqStl7CgZ8G4hXlQIc6m1BWaePq1beRCIHEj+h4Of5XiA/nsUk080ff6FwTM6i82P4TE59sbn4Qwtwu/+xNHUO6j3kfIRhR3amIsEeRdpDaX42YvVqVtquCNHQmcqeTSNqfwUKkZKP51tNqvGumPGbtcnQEYEeGOrOv0LOQ4YC83zjnOSYuWfwZ9QxI0FNi9QGG61BtZWmv2pML+AjuGKwaXQsGkFk2Z0JYCyQdYYeOq6jWrefdAAzbPUN9p8QSP5890tS7GgC9f8yQCspz7Ru92/9JO7pM9CthF/PLYIQHa7YIUvLNBN jon@jon-laptop" 
  ];
}

