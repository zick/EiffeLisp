class NUM

inherit
  LOBJ

create
  make_num

feature
  data: INTEGER

  make_num(n: INTEGER)
    do
      data := n
    end

end
