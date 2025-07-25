{ pkgs }:

let
  myAspellDicts = with pkgs.aspellDicts; [
    en
  ];

  myHunspellDicts = with pkgs.hunspellDicts; [
    en_US
    en_CA
  ];

  emacsAspell = pkgs.aspellWithDicts (aspellDicts: myAspellDicts);

  emacsHunspell = pkgs.hunspell.withDicts (hunspellDicts: myHunspellDicts);

in pkgs.buildEnv {
  name = "my-spelling";
  paths =
    # For Emacs enclosure
    (with pkgs; [
      emacsAspell
      emacsHunspell
    ])

    # For other applications that search for $PROFILE/share/hunspell
    ++ myAspellDicts
    ++ myHunspellDicts;
}
