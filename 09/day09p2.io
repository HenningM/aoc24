stdio := File standardInput
line := stdio readLine

files := List clone
free := List clone
c := 0
for (i, 0, (line size) - 1,
  val := line at(i) asCharacter asNumber
  begin := c
  end := c + val
  newL := list(begin, end)
  if (i % 2 == 0,
    files append(newL),
    if (val > 0, free append(newL))
  )
  c = c + val
)

ptr := (files size) - 1

while (ptr >= 0,
  seg := files at(ptr)
  begin := seg at(0)
  end := seg at(1)
  len := end - begin

  freeP := 0
  while (freeP < (free size),
    freeSeg := free at(freeP)
    freeBegin := freeSeg at(0)
    freeEnd := freeSeg at(1)
    freeLen := freeEnd - freeBegin

    if (freeBegin < begin,
      if (freeLen >= len,
        free removeAt(freeP)
        newBegin := freeBegin
        newEnd := freeBegin + len
        newFreeBegin := newEnd
        newF := list(newBegin, newEnd)
        files atPut(ptr, newF)

        if (newFreeBegin != freeEnd,
          free atInsert(freeP, list(newFreeBegin, freeEnd))
        )
        freeP = (free size),
        freeP = freeP + 1
      ),
      freeP = (free size)
    )
  )
  ptr = ptr - 1
)

(files size) repeat(i,
  seg := files at(i)
  begin := seg at(0)
  end := seg at(1)
  len := end - begin
  len repeat(j,
    sum := (i * (j + begin))
    sum println
  )
)
