let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          project1 =
            haskellPackagesNew.callPackage ./default.nix { };

          optparse-applicative =
            haskellPackagesNew.callPackage ./optparse-applicative-2.nix { };

          QuickCheck =
	    haskellPackagesNew.callPackage ./QuickCheck.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { project1 = pkgs.haskellPackages.project1;
  }
