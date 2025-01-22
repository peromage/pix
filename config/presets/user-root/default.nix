{ lib, ... }:

{
  config.pix.users.root = with lib; {
    nologin = true;
  };
}
