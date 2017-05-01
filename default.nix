{ stdenv, fetchFromGitHub, rustPlatform}:

rustPlatform.buildRustPackage rec {
  name = "pi-${version}";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "vmchale";
    repo = "project-init";
    rev = "${version}";
    sha256 = "1msm6pfxqrcwfhz71q1zdh7krm0s4pagsaqwr91h5vas9cff6h09";
    };

  depsSha256 = "1lg1jh6f9w28i94vaj62r859g6raalxmxabvw7av6sqr0hr56p05";

  installPhase = ''
    mkdir -p $out/bin
    cp -p target/release/tw $out/bin/
  '';

  meta = with stdenv.lib; {
    description = "Project initialization from templates";
    homepage = https://github.com/vmchale/project-init;
    license = licenses.bsd3;
    platforms = platforms.all;
  };
}
