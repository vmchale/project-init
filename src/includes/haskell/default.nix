{ mkDerivation, base, hspec, stdenv }:
mkDerivation {
  pname = "{{ project }}";
  version = "{{ version }}";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  homepage = "https://github.com/{{ github_username }}/{{ project }}#readme";
  license = stdenv.lib.licenses.bsd3;
}
