let pkg : { bin : List { src : Text, target : Text, libs : List Text, gc : Bool }, test : List { src : Text, target : Text, libs : List Text, gc : Bool }, man : Optional Text }
  = { bin = 
      [
        { src = "src/{{ project }}.dats"
        , target = "target/{{ project }}" 
        , libs = ([] : List Text)
        , gc = False
        }
      ]
    , test = [] 
        : List { src : Text, target : Text, libs : List Text, gc : Bool }
    , man = ([] : Optional Text)
    }

in pkg
