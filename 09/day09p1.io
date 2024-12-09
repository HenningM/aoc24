stdio := File standardInput
line := stdio readLine

expanded := List clone
for (i, 0, (line size) - 1,
  if (i % 2 == 0,
    (line at(i) asCharacter asNumber) repeat(expanded append((i / 2) asString)),
    (line at(i) asCharacter asNumber) repeat(expanded append("."))
  )
)

leftIt := 0
rightIt := (expanded size) - 1
while (leftIt < rightIt,
  while ((expanded at(leftIt)) != ".",
    leftIt = leftIt + 1
  )
  while ((expanded at(rightIt)) == ".",
    rightIt = rightIt - 1
  )
  expanded atPut(leftIt, (expanded at(rightIt)))
  expanded atPut(rightIt, ".")
  leftIt = leftIt + 1
  rightIt = rightIt - 1
)
sliced := expanded select(x, x != ".")
for (i, 0, (sliced size) - 1, ((sliced at(i) asNumber) * i) println)
