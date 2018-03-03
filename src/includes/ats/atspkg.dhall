let prelude = https://raw.githubusercontent.com/vmchale/atspkg/master/ats-pkg/dhall/atspkg-prelude.dhall

in prelude.default //
  { bin = 
    [ prelude.bin //
      { src = "src/{{ project }}.dats"
      , target = "target/{{ project }}" 
      }
    ]
  }
