{
  description = "Example kickstart Rust application project.";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];

      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: let
        inherit (pkgs) dockerTools rustPlatform;
        inherit (dockerTools) buildImage;
        inherit (rustPlatform) buildRustPackage;
        name = "u-lang";
        version = "0.1.0";
      in {
        devShells = {
          default = pkgs.mkShell {
            inputsFrom = [self'.packages.default];
          };
        };

        packages = {
          default = buildRustPackage {
            inherit version;
            cargoSha256 = "sha256-5eKMXR/s0Ut6TpKUIrg9MbmYTYToPhd8eGNPwEwomI8=";
            pname = name;
            buildInputs = with pkgs; [fasm];
            src = ./.;
          };

          docker = buildImage {
            inherit name;
            tag = version;
            config = {
              Cmd = ["${self'.packages.default}/bin/${name}"];
              Env = [
                "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
              ];
            };
          };
        };
      };
    };
}
