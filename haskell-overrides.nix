final: prev:
{
  haskell = prev.haskell // {
    packageOverrides = hfinal: hprev:
      with final.haskell.lib; {
      };
  };
}
