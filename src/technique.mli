open Statement

type object_phrase = string list

type technique = 
    |Refl
    |Rw of object_phrase

val next_statement: Statement.t -> technique -> Statement.t