{ghc}:

let pinned-nixpkgs-path = import ./pinned-nixpkgs.nix;
    pinned-pkgs = import pinned-nixpkgs-path {};
in

with pinned-pkgs;

(haskell.lib.buildStackProject {
  #inherit ghc;
  ghc = ghc;
  NIX_PATH = pinned-nixpkgs-path;
  name = "lin-lang";
  buildInputs = [ zlib ];
})
