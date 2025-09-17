final: prev:
{
  haskell = prev.haskell // {
    packageOverrides = hfinal: hprev:
      with final.haskell.lib; {
        microlens = overrideCabal hprev.microlens (old: {
          version = "0.4.14.0";
          sha256 = "sha256-VnkqYT5PsGNL3t9U3LdzrEuq6L4P7IvYjhu4t6ZJki4=";
          revision = null;
          editedCabalFile = null;
        });
      };
  };
}
