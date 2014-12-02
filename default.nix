{ pkgs ? (import <nixpkgs> {})
, haskellPackages ? pkgs.haskellPackages_ghc783
}:

haskellPackages.cabal.mkDerivation (self: {
  pname = "hm-typecheck";
  version = "0.1.0";
  src = ./.;
  buildDepends = with haskellPackages; [ cabalInstall hspec reactiveBanana ];
  hyperlinkSource = false;
  isLibrary = true;
  isExecutable = false;
})
