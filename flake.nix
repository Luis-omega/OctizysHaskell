{
  description = "A JAM implementation in Haskell by the ConfiturePeanut team.";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.treefmt-nix.url = "github:numtide/treefmt-nix";

  outputs =
    inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides =
            hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev
              // {
              pnrg = hfinal.callCabal2nix "pnrg" ./. { };
            };
        };
        pnrg = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.pnrg;
      };
      perSystem =
        system:
        let
          pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [ overlay ];
          };
          hspkgs = pkgs.haskellPackages;
          treefmtEval = inputs.treefmt-nix.lib.evalModule pkgs ./treefmt.nix;
          project_root = ./.;
          spell-check =
            pkgs.runCommandLocal "spell-check"
              {
                src = ./.;
                nativeBuildInputs = with pkgs; [ pkgs.typos ];
              }
              ''
                cd ${project_root}
                typos
                mkdir $out
              '';
        in
        {
          devShell = hspkgs.shellFor {
            withHoogle = true;
            packages = p: [ p.pnrg ];
            buildInputs = [
              hspkgs.cabal-install
              hspkgs.cabal-fmt
              hspkgs.haskell-language-server
              hspkgs.hlint
              hspkgs.fourmolu
              pkgs.bashInteractive
              pkgs.mdformat
              pkgs.typos
            ];
          };
          defaultPackage = pkgs.pnrg;
          formatter = treefmtEval.config.build.wrapper;
          checks = {
            formatting = treefmtEval.config.build.check inputs.self;
            inherit spell-check;
          };
          packages = {
            format-check = treefmtEval.config.build.check inputs.self;
            inherit spell-check;
          };
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
