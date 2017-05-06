{ mkDerivation, base, hspec, stdenv }:
mkDerivation {
  pname = "{{cookiecutter.project_slug}}";
  version = "{{cookiecutter.version}}";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  homepage = "https://github.com/{{cookiecutter.github_username}}/{{cookiecutter.project_slug}}#readme";
  license = stdenv.lib.licenses.bsd3;
}
