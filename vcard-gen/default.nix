{ mkDerivation, base, bytestring, case-insensitive, conformance
, conformance-gen, criterion, dlist, genvalidity
, genvalidity-bytestring, genvalidity-case-insensitive
, genvalidity-containers, genvalidity-criterion
, genvalidity-network-uri, genvalidity-sydtest, genvalidity-text
, genvalidity-time, genvalidity-vector, lib, megaparsec, path
, QuickCheck, sydtest, sydtest-discover, text, vcard
}:
mkDerivation {
  pname = "vcard-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring case-insensitive conformance conformance-gen dlist
    genvalidity genvalidity-bytestring genvalidity-case-insensitive
    genvalidity-containers genvalidity-network-uri genvalidity-sydtest
    genvalidity-text genvalidity-time path QuickCheck sydtest text
    vcard
  ];
  testHaskellDepends = [
    base bytestring conformance conformance-gen dlist
    genvalidity-sydtest genvalidity-text megaparsec path QuickCheck
    sydtest text vcard
  ];
  testToolDepends = [ sydtest-discover ];
  benchmarkHaskellDepends = [
    base criterion genvalidity-criterion genvalidity-vector vcard
  ];
  license = "unknown";
}
