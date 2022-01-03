let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
          ghcid
        ]);
    source-overrides = {
      plotlyhs = builtins.fetchTarball "https://github.com/JonathanReeve/plotlyhs/archive/0fcf833.tar.gz";
      Spock = builtins.dontCheck (builtins.fetchTarball "https://github.com/agrafix/Spock/archive/a1c7aea.tar.gz");
    };
  }
