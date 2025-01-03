import java.io.File

val fileName = "testData.txt"
//val fileName = "realData.txt"

fun pathToString(path: String): String = File(path).inputStream().bufferedReader().readText()

val stones = pathToString(fileName).split(" ").map { it.toLong() }

fun blink(stone: Long): List<Long> {
    val str = stone.toString()
    return when {
        stone == 0L -> listOf(1)
        str.length % 2 == 0 -> str.chunked(str.length / 2).map(String::toLong)
        else -> listOf(stone * 2024)
    }
}

fun List<Long>.blink(): List<Long> = flatMap(::blink)

tailrec fun <T> doTimes(n: Int, target: T, transform: (T) -> (T)): T =
    if (n < 1)
        target
    else
        doTimes(n - 1, transform(target), transform)

fun List<Long>.blink(times: Int): List<Long> = doTimes(times, this, { it.blink() })

// part 1
stones.blink(25).size.let(::println)


// part 2
stones.blink(75).size.let(::println)