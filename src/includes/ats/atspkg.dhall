let pkg : { bin : List { src : Text, target : Text, libs : List Text }, test : List { src : Text, target : Text, libs : List Text }, man : Optional Text }
  = { bin = 
      [
        { src = "src/{{ project }}.dats"
        , target = "target/{{ project }}" 
        , libs = ([] : List Text)
        }
      ]
    , test = [] 
        : List { src : Text, target : Text, libs : List Text }
    , man = ([] : Optional Text)
    }

in pkg
