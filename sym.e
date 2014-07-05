class SYM

inherit
  LOBJ

create
  make_sym

feature
  data: STRING

  make_sym(s: STRING)
    do
      data := s
    end

end
