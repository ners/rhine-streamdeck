{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-filter.url = "github:numtide/nix-filter";
    dunai = {
      url = "github:ivanperez-keera/dunai";
      flake = false;
    };
    rhine = {
      url = "github:ners/rhine/dunai-0.12";
      flake = false;
    };
    streamdeck = {
      url = "github:Linschlager/haskell-streamdeck/generalise-read";
      flake = false;
    };
  };

  outputs = inputs:
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else error "foreach: expected list or attrset but got ${builtins.typeOf xs}"
      );
      hsSrc = pname: root: inputs.nix-filter {
        inherit root;
        include = [
          "app"
          "lib"
          "test"
          "${pname}.cabal"
          "CHANGELOG.md"
        ];
      };
      pname = "rhine-streamdeck";
    in
    foreach inputs.nixpkgs.legacyPackages (system: pkgs:
      let
        defaultGhc = builtins.replaceStrings ["-" "."] ["" ""] pkgs.haskellPackages.ghc.name;
      in
      lib.recursiveUpdate
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          packages.${system}.default = inputs.self.packages.${system}."${pname}-${defaultGhc}";
          devShells.${system}.default = inputs.self.devShells.${system}.${defaultGhc};
        }
        (foreach (lib.filterAttrs (name: _: builtins.match "ghc[0-9]+" name != null) pkgs.haskell.packages)
          (ghcName: haskellPackages:
            let
              hp = haskellPackages.override {
                overrides = self: super:
                  #with pkgs.haskell.lib.compose;
                  {
                    dunai = self.callCabal2nix "dunai" "${inputs.dunai}/dunai" { };
                    bearriver = self.callCabal2nix "bearriver" "${inputs.dunai}/dunai-frp-bearriver" { };
                    rhine = self.callCabal2nix "rhine" "${inputs.rhine}/rhine" { };
                    rhine-terminal = self.callCabal2nix "rhine-terminal" "${inputs.rhine}/rhine-terminal" { };
                    streamdeck = self.callCabal2nix "streamdeck" inputs.streamdeck { };
                    ${pname} = self.callCabal2nix pname (hsSrc pname ./.) { };
                  };
              };
            in
            {
              packages.${system}."${pname}-${ghcName}" = hp.${pname};
              devShells.${system}.${ghcName} = hp.shellFor {
                packages = ps: [ ps.${pname} ];
                nativeBuildInputs = with hp; [
                  cabal-install
                  fourmolu
                  haskell-language-server
                ];
              };
            }
          )
        ));
}
