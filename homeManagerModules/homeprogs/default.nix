{ pix, ... }:

{
  imports = with pix.lib; listDir (notPred isDefaultNix) ./.;
}
