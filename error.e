class ERROR

inherit
  LOBJ

create
  make_error

feature
  data: STRING

  make_error(s: STRING)
    do
      data := s
    end

end
