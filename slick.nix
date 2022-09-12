{ mkDerivation, aeson, base, bytestring, directory, extra, fetchgit
, hpack, lib, mustache, pandoc, shake, text, unordered-containers
}:
mkDerivation {
  pname = "slick";
  version = "1.1.2.2";
  src = fetchgit {
    url = "https://github.com/ChrisPenner/slick.git";
    sha256 = "0xw0h022fdgh9liza831s0c679d6jcc1iv162rc7frnzg131wmxq";
    rev = "0127e8d7942dba17967d6ef431093f730e00d5e3";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson base bytestring directory extra mustache pandoc shake text
    unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "https://github.com/ChrisPenner/slick#readme";
  description = "A quick & easy static site builder built with shake and pandoc";
  license = lib.licenses.bsd3;
}
