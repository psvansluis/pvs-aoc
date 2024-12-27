import java.io.File

fun <T> List<T>.span(predicate: (T) -> Boolean): (Pair<List<T>, List<T>>) =
    takeWhile(predicate) to dropWhile(predicate)

//val fileName = "testData.txt"
val fileName = "realData.txt"

fun pathToString(path: String): String = File(path).inputStream().bufferedReader().readText()

data class CalibrationEquation(val testValue: Long, val equations: List<Long>) {
    fun canBeProduced(operations: List<(Long, Long) -> Long>): Boolean =
        makeEquation(0, equations, operations)

    private fun makeEquation(
        acc: Long,
        equations: List<Long>,
        operations: List<(Long, Long) -> Long>,
    ): Boolean = when {
        equations.isEmpty() -> acc == testValue
        acc > testValue -> false
        else -> operations.any { makeEquation(it(acc, equations.first()), equations.drop(1), operations) }
    }
}


val data = pathToString(fileName)
    .split("\n")
    .map {
        val splitTestValue = it.split(": ")
        val testValue = splitTestValue[0].toLong()
        val equations = splitTestValue[1].split(" ").map(String::toLong)
        return@map CalibrationEquation(testValue, equations)
    }

val part1Operations: List<(Long, Long) -> Long> = listOf(
    { a, b -> a + b },
    { a, b -> a * b },
)

val part1Answer = data
    .filter { it.canBeProduced(part1Operations) }
    .sumOf(CalibrationEquation::testValue)

println(part1Answer)

// part 2

val part2Operations: List<(Long, Long) -> Long> =
    part1Operations + { a, b -> (a.toString() + b.toString()).toLong() }

val part2Answer = data
    .filter { it.canBeProduced(part2Operations) }
    .sumOf(CalibrationEquation::testValue)

println(part2Answer)