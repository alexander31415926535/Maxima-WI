{ mkDerivation, aeson-compat, attoparsec, base, base-compat
, bytestring, extra, http-media, lucid, mtl, network, process
, servant-server, stdenv, string-conversions, texmath, text, unix
, wai, warp, xml, hxt
}:
mkDerivation {
  pname = "Maxima-WI";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson-compat attoparsec base base-compat bytestring extra
    http-media lucid mtl network process servant-server
    string-conversions texmath text unix wai warp xml hxt
  ];
  description = "Server providing HTTP access to Maxima-CAS";
  license = stdenv.lib.licenses.gpl3;
}
