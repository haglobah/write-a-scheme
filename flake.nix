{
  inputs = {
    nixpkgs.url = "https://flakehub.com/f/DeterminateSystems/nixpkgs-weekly/*.tar.gz";
    mission-control.url = "github:Platonic-Systems/mission-control";
    flake-root.url = "github:srid/flake-root";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.flake-root.flakeModule
        inputs.mission-control.flakeModule
      ];

      perSystem = { config, self', pkgs, ... }: {

        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
        haskellProjects.default = {
          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://community.flake.parts/haskell-flake/package-set
          # basePackages = pkgs.haskellPackages;

          # Extra package information. See https://community.flake.parts/haskell-flake/dependency
          #
          # Note that local packages are automatically included in `packages`
          # (defined by `defaults.packages` option).
          #
          packages = {
            # aeson.source = "1.5.0.0"; # Hackage version override
            # shower.source = inputs.shower;
          };
          settings = {
            #  aeson = {
            #    check = false;
            #  };
            #  relude = {
            #    haddock = false;
            #    broken = false;
            #  };
          };

          autoWire = ["packages" "checks" "apps"];
          devShell = {
            # Enabled by default
            # enable = true;

            # Programs you want to make available in the shell.
            # Default programs can be disabled by setting to 'null'
            # tools = hp: { fourmolu = hp.fourmolu; ghcid = null; };

            hlsCheck.enable = pkgs.stdenv.isDarwin; # On darwin, sandbox is disabled, so HLS can use the network.
          };
        };

        apps.app1 = self'.apps.write-a-scheme;

        checks = {
          parser = pkgs.runCommandNoCC "the-parser"
          {
            nativeBuildInputs = self'.devShells.default.nativeBuildInputs;
          }
          ''
            echo "Here come the tests"
            echo "-->"
            # echo "${self'.apps.app1.program}"
            [[ `${self'.apps.app1.program} '()'` == "Unrecognized special form: ()" ]]
            [[ `${self'.apps.app1.program} '(* 2 3)'` == "6" ]]
            echo "--> Tests end."
            touch $out
          '';
        };

        mission-control = {
          # scripts = {
          #   nr = { exec = "nix run . "; description = "nix run Haskell binary"; };
          #   nl = { exec = "nix run -L . "; description = "nix run Haskell binary with logging enabled";};
          # };
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.flake-root.devShell
            config.mission-control.devShell
          ];
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.write-a-scheme;
      };
    };
}
