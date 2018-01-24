let pkg = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default.dhall

in pkg //
  { bin = 
    [
      { src = "src/{{ project }}.dats"
      , target = "target/{{ project }}" 
      , libs = [ "pthread" ]
      , gc = False
      }
    ]
  }
