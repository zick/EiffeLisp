class CONS

inherit
  LOBJ

create
  make_cons

feature
  car: LOBJ
  cdr: LOBJ

  make_cons(a: LOBJ; d: LOBJ)
    do
      car := a
      cdr := d
    end

end
