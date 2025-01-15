{ pkgs }:

let
  myAspell = pkgs.aspellWithDicts (aspellDicts: with aspellDicts; [
    en
  ]);

  myHunspell = pkgs.hunspellWithDicts (with pkgs.hunspellDicts; [
    en_US
  ]);

in pkgs.buildEnv {
  name = "my-spelling-and-dicts";
  paths = with pkgs; [
    myAspell
    myHunspell
  ];
}
