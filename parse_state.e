class PARSE_STATE

create
  make

feature
  obj: LOBJ
  next: STRING

  make(o: LOBJ; s: STRING)
    do
      obj := o
      next := s
    end

end