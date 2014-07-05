class SUBR

inherit
  LOBJ

create
  make_subr

feature
  id: INTEGER

  make_subr(i: INTEGER)
    do
      id := i
    end

end
