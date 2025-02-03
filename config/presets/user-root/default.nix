{ lib, ... }:

{
  config.pix.users.root = {
    nologin = true;
  };
}
