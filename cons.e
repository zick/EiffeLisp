class CONS

inherit
  LOBJ

create
  make_cons

feature
  car: LOBJ assign set_car
  cdr: LOBJ assign set_cdr

  set_car(x: LOBJ)
    do
      car := x
    end

  set_cdr(x: LOBJ)
    do
      cdr := x
    end

  make_cons(a: LOBJ; d: LOBJ)
    do
      car := a
      cdr := d
    end

end
