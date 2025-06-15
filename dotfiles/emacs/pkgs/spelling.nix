{ pkgs }:

let
  myAspell = pkgs.aspellWithDicts (aspellDicts: with aspellDicts; [
    en
  ]);

  myHunspell = pkgs.hunspellWithDicts (with pkgs.hunspellDicts; [
    en_US
    en_CA
  ]);

in pkgs.buildEnv {
  name = "my-spelling";
  paths = with pkgs; [
    myAspell
    myHunspell
  ];
}
