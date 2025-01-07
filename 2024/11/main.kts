import java.io.File


//val fileName = "testData.txt"
val fileName = "realData.txt"

fun pathToString(path: String): String = File(path).inputStream().bufferedReader().readText()

val stones = pathToString(fileName).split(" ").map(String::toLong)

fun blink(stone: Long): List<Long> {
    val str = stone.toString()
    return when {
        stone == 0L -> listOf(1L)
        str.length % 2 == 0 -> str.chunked(str.length / 2).map(String::toLong)
        else -> listOf(stone * 2024L)
    }
}

val memo = mutableMapOf<Pair<List<Long>, Int>, Long>()
fun <I, O> memoize(memo: MutableMap<I, O>, f: (I) -> O): (I) -> O = { input ->
    memo.getOrPut(input) { f(input) }
}

fun countStones(stonesAndBlinks: Pair<List<Long>, Int>): Long = memoize(memo) { (stones, blinks) ->
    when {
        stones.isEmpty() || blinks < 1 -> stones.size.toLong()
        else -> stones.sumOf { countStones(blink(it) to blinks - 1) }
    }
}(stonesAndBlinks)


// part 1
val part1 = countStones(stones to 25)

// part 2
val part2 = countStones(stones to 75)

println("part1: $part1, part2: $part2")
println("memo size: ${memo.size}")