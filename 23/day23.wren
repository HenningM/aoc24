import "io" for Stdin

var readInput = Fn.new {
  var pairs = []
  var line = ""

  while ((line = Stdin.readLine()) != "") {
    pairs = pairs + [line.split("-")]
  }
  return pairs
}

var buildConnectionMap = Fn.new { |pairs|
  var map = {}

  for (p in pairs) {
    var p1 = p[0]
    var p2 = p[1]

    map[p1] = (map[p1] || []) + [p2]
    map[p2] = (map[p2] || []) + [p1]
  }

  return map
}

var findSets = Fn.new { |connectionMap|
  var visited = {}
  var sets = {}
  var computers = connectionMap.keys.toList
  for (c in computers) {
    visited[c] = true
    var conns = connectionMap[c].where { |m| visited[m] == null }
    for (n in conns) {
      var overlap = connectionMap[n].where { |m| visited[m] == null && conns.contains(m) }
      for (o in overlap) {
        var key = [c, n, o].sort { |a, b| computers.indexOf(a) < computers.indexOf(b) }.join(",")
        sets[key] = true
      }
    }
  }
  return sets.keys.map { |x| x.split(",") }.toList
}

var compareStrings = Fn.new { |a, b|
  for (i in 0...a.count) {
    if (a.codePoints[i] > b.codePoints[i]) {
      return false
    }
    if (a.codePoints[i] < b.codePoints[i]) {
      return true
    }
  }
  return true
}

var findBiggestNetwork

findBiggestNetwork = Fn.new { |max, compSet, excludes|
  var biggest = []
  if (compSet.count == 0 && excludes.count == 0) {
    return max
  }
  for (c in compSet) {
    var newMax = max + [c.key]
    var newCompSet = {}
    for (x in compSet) {
      if (c.value.contains(x.key)) {
        newCompSet[x.key] = x.value
      }
    }
    var newExcludes = excludes.where { |i| c.value.contains(i) }.toList

    var newNetwork = findBiggestNetwork.call(newMax, newCompSet, newExcludes)
    if (newNetwork.count > biggest.count) {
      biggest = newNetwork
    }
    compSet.remove(c.key)
    excludes = excludes + [c.key]
  }
  return biggest
}

var solvePartOne = Fn.new { |input|
  var connectionMap = buildConnectionMap.call(input)
  var sets = findSets.call(connectionMap)
  var tCompSets = sets.where { |c| c.any { |i| i.startsWith("t") } }
  return tCompSets.count
}

var solvePartTwo = Fn.new { |input|
  var connectionMap = buildConnectionMap.call(input)
  var network = findBiggestNetwork.call([], connectionMap, [])
  network.sort { |a, b| compareStrings.call(a, b) }
  return network.join(",")
}

var input = readInput.call()
var partOne = solvePartOne.call(input)
System.print(partOne)
var partTwo = solvePartTwo.call(input)
System.print(partTwo)
