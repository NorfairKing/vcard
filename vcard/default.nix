{ mkDerivation, base, bytestring, case-insensitive, conformance
, containers, deepseq, dlist, lib, megaparsec, network-uri, text
, time, validity, validity-case-insensitive, validity-containers
, validity-network-uri, validity-text, validity-time
}:
mkDerivation {
  pname = "vcard";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring case-insensitive conformance containers deepseq
    dlist megaparsec network-uri text time validity
    validity-case-insensitive validity-containers validity-network-uri
    validity-text validity-time
  ];
  license = "unknown";
}
