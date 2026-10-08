{
  pkgs,
  userNpmDir ? "npm-packages",
}: let
  myNodeJs = pkgs.nodejs_latest;
in
  pkgs.buildEnv {
    name = "pix-nodejs";
    paths = [myNodeJs];

    passthru = {
      userNpmDir = userNpmDir;
      userPath = "${userNpmDir}/bin";
    };
  }
