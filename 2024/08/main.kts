import java.io.File

//val fileName = "testData.txt"
val fileName = "realData.txt"

fun pathToString(path: String): String = File(path).inputStream().bufferedReader().readText()

data class Point(val x: Int, val y: Int) {

    private fun antinodesAfter(other: Point): Sequence<Point> =
        generateSequence(other) { Point(it.x + other.x - x, it.y + other.y - y) }

    fun antinodes(other: Point): Pair<Sequence<Point>, Sequence<Point>> =
        this.antinodesAfter(other) to other.antinodesAfter(this)
}

fun <T> List<T>.pairCombinations(): List<Pair<T, T>> =
    when (val head = firstOrNull()) {
        null -> listOf()
        else -> with(drop(1)) { map { head to it } + pairCombinations() }
    }

data class AntennaMap(val xSize: Int, val ySize: Int, val antennae: Map<Char, Set<Point>>) {
    fun fitsOnMap(p: Point): Boolean = p.x in (0 until xSize) && p.y in (0 until ySize)

    fun List<Point>.antinodes(f: (Sequence<Point>) -> Sequence<Point>): Set<Point> =
        this.pairCombinations().fold(emptySet()) { set, (p1, p2) ->
            val (a1, a2) = p1.antinodes(p2)
            val s = { s: Sequence<Point> -> f(s.takeWhile(::fitsOnMap)) }
            set + s(a1) + s(a2)
        }

    fun countAntinodes(f: (Sequence<Point>) -> Sequence<Point>) =
        antennae
            .values
            .flatMap { it.toList().antinodes(f) }
            .distinct()
            .count()
}

val data: AntennaMap =
    pathToString(fileName)
        .split("\n")
        .let {
            val xSize = it[0].length
            val ySize = it.size
            val antennae =
                it.mapIndexed { y, row ->
                    row.mapIndexed { x, c ->
                        when (c) {
                            '.' -> null
                            else -> c to Point(x, y)
                        }
                    }
                }
                    .flatten()
                    .filterNotNull()
                    .fold(mapOf<Char, Set<Point>>()) { acc, (k, v) ->
                        acc + (k to (acc[k] ?: emptySet()) + v)
                    }
            AntennaMap(xSize, ySize, antennae)
        }

listOf<(Sequence<Point>) -> Sequence<Point>>(
    { a -> a.drop(1).take(1) },
    { a -> a })
    .forEachIndexed { i, f -> println("Antinodes for part ${i + 1}: ${data.countAntinodes(f)}") }
