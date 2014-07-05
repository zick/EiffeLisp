class EIFFELISP

create
  make

feature

  kLPar: CHARACTER = '('
  kRPar: CHARACTER = ')'
  kQuote: CHARACTER = '%''

  kNil: LOBJ
  sym_quote: LOBJ

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

  makeCons(a: LOBJ; d: LOBJ): LOBJ
    local
      c: CONS
    do
      create c.make_cons(a, d)
      Result := c
    end

  nreverse(l: LOBJ): LOBJ
    local
      lst: LOBJ
      tmp: LOBJ
    do
      Result := kNil
      from
        lst := l
      until
        lst = kNil
      loop
        if attached {CONS} lst as c then
          tmp := c.cdr
          c.cdr := Result
          Result := lst
          lst := tmp
        else
          lst := kNil  -- break
        end
      end
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

  readAtom(str: STRING): PARSE_STATE
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
      create Result.make(makeNumOrSym(s), next)
    end

  parseError(s: STRING): PARSE_STATE
    do
      create Result.make(makeError(s), "")
    end

  read(str: STRING): PARSE_STATE
    local
      s: STRING
      tmp: PARSE_STATE
    do
      s := skipSpaces(str)
      if s.count = 0 then
        Result := parseError("empty input")
      elseif s[1] = kRPar then
        Result := parseError("invalid syntax: " + s)
      elseif s[1] = kLPar then
        Result := readList(s.substring(2, s.count))
      elseif s[1] = kQuote then
        tmp := read(s.substring(2, s.count))
        create Result.make(makeCons(sym_quote, makeCons(tmp.obj, kNil)),
                           tmp.next)
      else
        Result := readAtom(s)
      end
    end

  readList(str: STRING): PARSE_STATE
    local
      obj: LOBJ
      s: STRING
      next: STRING
      tmp: PARSE_STATE
    do
      next := ""
      obj := kNil
      from
        s := str
      until
        s.count = 0
      loop
        s := skipSpaces(s)
        if s.count = 0 then
          obj := makeError("unfinished parenthesis")
          s := ""  -- break
        elseif s[1] = kRPar then
          next := s.substring(2, s.count)
          s := ""  -- break
        else
          tmp := read(s)
          if attached {ERROR} tmp.obj as err then
            obj := tmp.obj
            s := ""  -- break
          else
            obj := makeCons(tmp.obj, obj)
            s := tmp.next
          end
        end
      end
      if attached {CONS} obj then
        create Result.make(nreverse(obj), next)
      else
        create Result.make(obj, next)
      end
    end

  printObj(obj: LOBJ): STRING
    do
      if attached {NIL} obj then
        Result := "nil"
      elseif attached {NUM} obj as n then
        Result := n.data.out
      elseif attached {SYM} obj as s then
        Result := s.data
      elseif attached {ERROR} obj as e then
        Result := "<error: " + e.data + ">"
      elseif attached {CONS} obj then
        Result := printList(obj)
      else
        Result := "<unknown>"
      end
    end

  printList(obj: LOBJ): STRING
    local
      lst: LOBJ
      first: BOOLEAN
      done: BOOLEAN
    do
      Result := ""
      lst := obj
      from
        first := True
        done := False
      until
        done = True
      loop
        if attached {CONS} lst as c then
          if first then
            first := False
          else
            Result := Result + " "
          end
          Result := Result + printObj(c.car)
          lst := c.cdr
        else
          done := True -- break
        end
      end
      if lst = kNil then
        Result := "(" + Result + ")"
      else
        Result := "(" + Result + " . " + printObj(lst) + ")"
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
      sym_quote := makeSym("quote")
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
          print(printObj(read(line).obj))
          io.put_new_line
        end
      end
    end

end
