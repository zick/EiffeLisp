class EXPR

inherit
  LOBJ

create
  make_expr

feature
  args: LOBJ
  body: LOBJ
  env: LOBJ

  make_expr(a, b, e: LOBJ)
    do
      args := a
      body := b
      env := e
    end

end
