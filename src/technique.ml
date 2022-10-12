type object_phrase = string list

type technique = 
    |Refl
    |Rw of object_phrase

let next_statement x y = 
  raise (Failure "unimplemented next statement")