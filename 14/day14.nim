import std/strutils
import std/sequtils
import std/sugar

type Robot = object
  x: int
  y: int
  vx: int
  vy: int

proc readInput(): seq[Robot] =
  while true:
    var line = ""
    try:
      line = stdin.readLine()
    except EOFError:
      break
    var parts = line.split(" ")
    var pos = parts[0].split("=")[1].split(",")
    var vel = parts[1].split("=")[1].split(",")
    var x = parseInt(pos[0])
    var y = parseInt(pos[1])
    var vx = parseInt(vel[0])
    var vy = parseInt(vel[1])
    var robot = Robot(x: x, y: y, vx: vx, vy:vy)
    result.add(robot)

proc determineBoardSize(robots: seq[Robot]): (int, int) =
  if robots.len > 15:
    result = (101, 103)
  else:
    result = (11, 7)

proc tick(self: Robot, boardSize: (int, int), seconds: int): Robot =
  var copyR = Robot(x: self.x, y: self.y, vx: self.vx, vy: self.vy)
  copyR.x = (copyR.x + copyR.vx * seconds) mod boardSize[0]
  copyR.y = (copyR.y + copyR.vy * seconds) mod boardSize[1]
  if copyR.x < 0:
    copyR.x = boardSize[0] + copyR.x
  if copyR.y < 0:
    copyR.y = boardSize[1] + copyR.y
  result = copyR

proc printBoard(boardSize: (int, int), robots: seq[Robot]) =
  var board = newSeqWith(boardSize[1], newSeq[int](boardSize[0]))
  for y in 0..boardSize[1] - 1:
    for x in 0..boardSize[0] - 1:
      board[y][x] = 0
  for robot in robots:
    board[robot.y][robot.x] += 1
  for y in 0..boardSize[1] - 1:
    for x in 0..boardSize[0] - 1:
      stdout.write if board[y][x] == 0: "." else: "#"
    echo ""

proc solvePartOne(robots: seq[Robot]): int =
  var boardSize = determineBoardSize(robots)
  var newRobots = robots.mapIt(it.tick(boardSize, 100))
  var tl = newRobots.countIt(it.x < boardSize[0] div 2 and it.y < boardSize[1] div 2)
  var tr = newRobots.countIt(it.x > boardSize[0] div 2 and it.y < boardSize[1] div 2)
  var br = newRobots.countIt(it.x > boardSize[0] div 2 and it.y > boardSize[1] div 2)
  var bl = newRobots.countIt(it.x < boardSize[0] div 2 and it.y > boardSize[1] div 2)
  result = tl * tr * br * bl

proc countCentered(robots: seq[Robot], boardSize: (int, int)): int =
  var lCutoff = boardSize[0] div 4
  var rCutoff = boardSize[0] - lCutoff
  var tCutoff = boardSize[1] div 4
  var bCutoff = boardSize[1] - tCutoff
  result = robots.countIt(it.x > lCutoff and it.x < rCutoff and it.y > tCutoff and it.y < bCutoff)

proc solvePartTwo(robots: seq[Robot]): int =
  var boardSize = determineBoardSize(robots)
  var threshold = robots.len div 2
  var tickCount = 0
  var newRobots = robots.mapIt(it.tick(boardSize, tickCount))
  var centered = countCentered(newRobots, boardSize)
  while centered < threshold:
    tickCount += 1
    newRobots = robots.mapIt(it.tick(boardSize, tickCount))
    centered = countCentered(newRobots, boardSize)
  result = tickCount

proc day14() =
  var robots = readInput()
  var partOne = solvePartOne(robots)
  echo partOne

  var partTwo = solvePartTwo(robots)
  echo partTwo

day14()
