class EIFFELISP

create
  make

feature

  kLPar: CHARACTER = '('
  kRPar: CHARACTER = ')'
  kQuote: CHARACTER = '%''

  kNil: LOBJ

  safeCar(obj: LOBJ): LOBJ
    do
      if attached {CONS} obj as c then
        Result := c.car
      else
        Result := kNil
      end
    end

  safeCdr(obj: LOBJ): LOBJ
    do
      if attached {CONS} obj as c then
        Result := c.cdr
      else
        Result := kNil
      end
    end

  makeNum(n: Integer): LOBJ
    local
      num: NUM
    do
      create num.make_num(n)
      Result := num
    end

  sym_table: HASH_TABLE[LOBJ, STRING]
  makeSym(s: STRING): LOBJ
    local
      new_sym: SYM
    do
      if attached sym_table[s] as sym then
        Result := sym
      else
        create new_sym.make_sym(s)
        sym_table.put(new_sym, s)
        Result := new_sym
      end
    end

  makeError(s: STRING): LOBJ
    local
      err: ERROR
    do
      create err.make_error(s)
      Result := err
    end

  isSpace(c: CHARACTER): BOOLEAN
    do
      if c = '%T' or else c = '%R' or else c = '%N' or else c = ' ' then
        Result := True
      else
        Result := False
      end
    end

  isDelimiter(c: CHARACTER): BOOLEAN
    do
      if c = kLPar or else c = kRPar or else c = kQuote or else isSpace(c) then
        Result := True
      else
        Result := False
      end
    end

  skipSpaces(s: STRING): STRING
    local
      i: INTEGER
    do
      Result := ""
      from
        i := 1
      until
        i > s.count
      loop
        if not isSpace(s[i]) then
          Result := s.substring(i, s.count)
          i := s.count  -- break
        end
        i := i + 1
      end
    end

  makeNumOrSym(s: STRING): LOBJ
    do
      if s.is_integer then
        Result := makeNum(s.to_integer)
      else
        Result := makeSym(s)
      end
    end

  readAtom(str: STRING): TUPLE[LOBJ, STRING]
    local
      i: INTEGER
      s: STRING
      next: STRING
    do
      s := str
      next := ""
      from
        i := 1
      until
        i > s.count
      loop
        if isDelimiter(s[i]) then
          next := s.substring(i, s.count)
          s := s.substring(1, i - 1)
          i := s.count  -- break
        end
        i := i + 1
      end
      Result := [makeNumOrSym(s), next]
    end

  read(str: STRING): TUPLE[LOBJ, STRING]
    local
      s: STRING
    do
      s := skipSpaces(str)
      if s.count = 0 then
        Result := [makeError("empty input"), ""]
      elseif s[1] = kRPar then
        Result := [makeError("invalid syntax: " + s), ""]
      elseif s[1] = kLPar then
        Result := [makeError("noimpl"), ""]
      elseif s[1] = kQuote then
        Result := [makeError("noimpl"), ""]
      else
        Result := readAtom(s)
      end
    end

  init
    local
      nil: NIL
    do
      create nil.make_nil
      kNil := nil

      create sym_table.make(32)
      sym_table.put(kNil, "nil")
    end

  make
    local
      line: STRING
    do
      init
      from
        line := "dummy"
      until
        line.count = 0
      loop
        print("> ")
        io.read_line
        line := io.last_string
        if line.count > 0 then
          print(read(line)[1])
          io.put_new_line
        end
      end
    end

end
