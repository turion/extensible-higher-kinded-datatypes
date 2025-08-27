{
  description = "extensible-higher-kinded-datatypes";

  nixConfig = {
    extra-substituters = [
      "https://extensible-hkds.cachix.org"
    ];
    extra-trusted-public-keys = [
      "eextensible-hkds.cachix.org-1:+V2EIdfZC7bvEVQANRyvaGhqaGOBSRBiG7p6RZittSQ="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    with builtins;
    let
      lib = nixpkgs.lib;

      localPackages = { extensible-higher-kinded-datatypes = ./extensible-higher-kinded-datatypes; };

      # All GHC versions that this project is tested with.
      supportedGhcs = [
        "ghc810"
        "ghc90"
        "ghc92"
        "ghc94"
        "ghc96"
        "ghc98"
        "ghc910"
        "ghc912"
      ];

      # A haskellPackages overlay containing everything defined in this repo
      overrides = hfinal: hprev:
        lib.mapAttrs (pname: path: hfinal.callCabal2nix pname path { }) localPackages;

    in
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          # The Haskell packages set, for every supported GHC version
          haskellPackagesPerVersion =
            lib.genAttrs supportedGhcs (ghc: pkgs.haskell.packages.${ghc})
            // { default = pkgs.haskellPackages; };

          haskellPackagesPerVersionExtended = mapAttrs
            (_: haskellPackages:
              haskellPackages.override (_: {
                inherit overrides;
              })
            )
            haskellPackagesPerVersion;

        in
        {
          # Usage: nix fmt
          formatter = pkgs.nixpkgs-fmt;

          # This builds the main package on all GHCs
          # Usage: nix build
          packages = mapAttrs (_: haskellPackages: haskellPackages.extensible-higher-kinded-datatypes) haskellPackagesPerVersionExtended;

          # Usage: nix develop (will use the default GHC)
          # Alternatively, specify the GHC: nix develop .#ghc98
          devShells = mapAttrs
            (ghcVersion: hp: hp.shellFor {
              packages = ps: map (pname: ps.${pname}) (attrNames localPackages);
              nativeBuildInputs = (with haskellPackagesPerVersion.${ghcVersion};
                lib.optional (lib.versionAtLeast ghc.version "9.4") [
                  haskell-language-server
                ]) ++ (with pkgs; [
                fourmolu
              ]) ++ (with pkgs.haskellPackages; [
                cabal-install
                cabal-gild
              ]);
            })
            haskellPackagesPerVersionExtended;
        }
      ) // {
      inherit supportedGhcs;
    };
}
