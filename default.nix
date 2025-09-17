{ mkDerivation, base, bytestring, http-conduit, lib, microlens
, microlens-aeson, optparse-applicative, prettyprinter
, terminal-size, text, yaml
}:
mkDerivation {
  pname = "github-workflows";
  version = "0.1.0.0";
  src = lib.cleanSource ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring http-conduit microlens microlens-aeson
    optparse-applicative prettyprinter terminal-size text yaml
  ];
  description = "Utility to manage GitHub workflows for a user";
  license = lib.licenses.mit;
  mainProgram = "github-workflows";
}
