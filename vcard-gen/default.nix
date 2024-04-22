{ mkDerivation, base, bytestring, case-insensitive, conformance
, conformance-gen, containers, criterion, deepseq, dlist
, genvalidity, genvalidity-bytestring, genvalidity-case-insensitive
, genvalidity-containers, genvalidity-criterion
, genvalidity-network-uri, genvalidity-sydtest, genvalidity-text
, genvalidity-time, genvalidity-vector, lib, megaparsec
, network-uri, path, path-io, pretty-show, QuickCheck, sydtest
, sydtest-discover, text, time, validity-network-uri, vcard, vector
}:
mkDerivation {
  pname = "vcard-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring case-insensitive conformance conformance-gen
    containers dlist genvalidity genvalidity-bytestring
    genvalidity-case-insensitive genvalidity-containers
    genvalidity-network-uri genvalidity-sydtest genvalidity-text
    genvalidity-time path QuickCheck sydtest text time vcard
  ];
  testHaskellDepends = [
    base bytestring conformance conformance-gen containers dlist
    genvalidity genvalidity-sydtest genvalidity-text megaparsec
    network-uri path path-io pretty-show QuickCheck sydtest text time
    validity-network-uri vcard
  ];
  testToolDepends = [ sydtest-discover ];
  benchmarkHaskellDepends = [
    base criterion deepseq genvalidity genvalidity-criterion
    genvalidity-vector QuickCheck vcard vector
  ];
  license = "unknown";
}
