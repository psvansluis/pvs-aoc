import java.io.File
import kotlin.math.abs

val fileName = "realData.txt"
val inputStream = File(fileName).inputStream()
val inputString = inputStream.bufferedReader().readText()

val parsed = inputString.split("\n").map { line ->
    line.split(" ").map(String::toInt)
}

fun <T> List<T>.allAdjacent(predicate: (Pair<T, T>) -> Boolean): Boolean =
    this.windowed(2).map { Pair(it[0], it[1]) }.all(predicate)

fun List<Int>.isIncreasing() = allAdjacent { (fst, snd) -> fst < snd }

fun List<Int>.isDecreasing() = allAdjacent { (fst, snd) -> fst > snd }

fun List<Int>.differsByAtLeastOneAndAtMostThree() = allAdjacent { (fst, snd) ->
    abs(fst - snd) in 1..3
}

fun List<Int>.isSafe() = (isIncreasing() || isDecreasing()) && differsByAtLeastOneAndAtMostThree()

val safeLevels = parsed.count { it.isSafe() }
println("safe levels: $safeLevels")