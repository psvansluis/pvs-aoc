import java.io.File
import kotlin.math.abs

val fileName = "realData.txt"
//val fileName = "testData.txt"
val inputStream = File(fileName).inputStream()
val inputString = inputStream.bufferedReader().readText()

val parsed = inputString.split("\n").map { line ->
    line.split(" ").map(String::toInt)
}

fun <T> List<T>.allAdjacent(predicate: (Pair<T, T>) -> Boolean): Boolean =
    this.windowed(2).map { Pair(it[0], it[1]) }.all(predicate)

fun List<Int>.isIncreasing() = allAdjacent { it.first < it.second }

fun List<Int>.isDecreasing() = allAdjacent { it.first > it.second }

fun List<Int>.differsByAtLeastOneAndAtMostThree() = allAdjacent {
    abs(it.first - it.second) in 1..3
}

fun List<Int>.isSafe() = (isIncreasing() || isDecreasing()) && differsByAtLeastOneAndAtMostThree()

val safeLevels = parsed.count { it.isSafe() }
println("safe levels: $safeLevels")

// part 2

val List<Int>.subLists: List<List<Int>>
    get() = List(size) { i -> this.filterIndexed { index, _ -> index != i } }


val safeLevelsWithProblemDampener = parsed.count { line -> line.subLists.any { it.isSafe() } }

println("safe levels with problem dampener: $safeLevelsWithProblemDampener")
