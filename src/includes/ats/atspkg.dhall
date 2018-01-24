let pkg = https://raw.githubusercontent.com/vmchale/atspkg/master/atspkg.dhall

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
