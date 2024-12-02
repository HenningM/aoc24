proc day14() =
  while true:
    var line = ""
    try:
      line = stdin.readLine()
    except EOFError:
      break
    echo line

day14()
