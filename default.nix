{ mkDerivation, base, bytestring, http-conduit, lib, microlens
, microlens-aeson, optparse-applicative, prettyprinter
, terminal-size, yaml
}:
mkDerivation {
  pname = "github-repos";
  version = "0.1.0.0";
  src = lib.cleanSource ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring http-conduit microlens microlens-aeson
    optparse-applicative prettyprinter terminal-size yaml
  ];
  description = "Utilities to query GitHub repositories for a user";
  license = lib.licenses.mit;
  mainProgram = "github-repos";
}
