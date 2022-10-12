type expression = string list


type technique = 
    |Refl
    |Rw of expression

let next_statement x y = 
  raise (Failure "unimplemented next statement")