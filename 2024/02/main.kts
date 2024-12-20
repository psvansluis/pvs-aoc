import java.io.File
import kotlin.math.abs

typealias Line = List<Int>

val fileName = "realData.txt"
//val fileName = "testData.txt"
val inputStream = File(fileName).inputStream()
val inputString = inputStream.bufferedReader().readText()

val lines: List<Line> = inputString.split("\n").map {
    it.split(" ").map(String::toInt)
}

fun <T> List<T>.allAdjacent(predicate: (Pair<T, T>) -> Boolean): Boolean =
    this.windowed(2).map { Pair(it[0], it[1]) }.all(predicate)

fun Line.isIncreasing() = allAdjacent { it.first < it.second }

fun Line.isDecreasing() = allAdjacent { it.first > it.second }

fun Line.differsByAtLeastOneAndAtMostThree() = allAdjacent {
    abs(it.first - it.second) in 1..3
}

fun Line.isSafe() = (isIncreasing() || isDecreasing()) && differsByAtLeastOneAndAtMostThree()

val safeLevels = lines.count { it.isSafe() }
println("safe levels: $safeLevels")

// part 2

val Line.subLists: List<Line>
    get() = List(size) { i -> this.filterIndexed { index, _ -> index != i } }

val safeLevelsWithProblemDampener = lines.count { line -> line.subLists.any { it.isSafe() } }

println("safe levels with problem dampener: $safeLevelsWithProblemDampener")
