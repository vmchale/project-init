let pkg
  = { bin = 
      [
        { src = "src/{{ project }}.dats"
        , target = "target/poly" 
        , libs = [ "pthread" ]
        , gc = False
        }
      ]
    , test = [] 
        : List { src : Text, target : Text, libs : List Text, gc : Bool }
    , man = ([] : Optional Text)
    , version = [0,3,9]
    }

in pkg
