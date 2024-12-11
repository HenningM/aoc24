fun parseInput(input: String): List<Long> = input.split(" ").map({ it.toLong() })

val cache = mutableMapOf<Pair<Long, Int>, Long>()

fun cachedCountStones(stoneVal: Long, blinks: Int): Long {
  val result = cache.getOrPut(Pair(stoneVal, blinks)) {
    val stoneStr = stoneVal.toString()
    val stoneLen = stoneStr.length
    if (blinks == 0) {
      1
    } else if (stoneVal == 0L) {
      cachedCountStones(1, blinks - 1)
    } else if (stoneLen % 2 == 0) {
      val stone1 = stoneStr.slice(0..stoneLen/2-1).toLong()
      val stone2 = stoneStr.slice(stoneLen/2..stoneLen-1).toLong()
      cachedCountStones(stone1, blinks - 1) + cachedCountStones(stone2, blinks - 1)
    } else {
      cachedCountStones(stoneVal * 2024, blinks - 1)
    }
  }
  return result
}

fun main() {
  val input = readlnOrNull()
  val parsed = parseInput(input!!)
  var sum = 0L
  for (s in parsed.iterator()) {
    val count = cachedCountStones(s, 25)
    sum += count
  }
  println(sum)
  var sum2 = 0L
  for (s in parsed.iterator()) {
    val count = cachedCountStones(s, 75)
    sum2 += count
  }
  println(sum2)
}
