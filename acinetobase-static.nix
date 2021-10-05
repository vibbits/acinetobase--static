{ mkDerivation, aeson, base, bytestring, cassava, containers
, filepath, lens, lens-aeson, lib, mustache, shake, slick, text
, time, unordered-containers, vector
}:
mkDerivation {
  pname = "acinetobase-static";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring cassava containers filepath lens lens-aeson
    mustache shake slick text time unordered-containers vector
  ];
  description = "Acinetobase: Compendium of Experiments in the Lab";
  license = lib.licenses.gpl3Only;
}
