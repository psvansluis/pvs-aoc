import java.io.File

// Matrix ops

fun <T> transpose(matrix: List<List<T>>): List<List<T>> =
    matrix
        .getOrNull(0)
        ?.indices
        ?.map { i -> matrix.map { it[i] } }
        ?: emptyList()

fun <T> getMainDiagonals(matrix: List<List<T>>): List<List<T>> {
    val nRows = matrix.size
    val nCols = if (nRows > 0) matrix[0].size else 0

    return (0 until nRows + nCols - 1).mapNotNull { diagonalIndex ->
        val (startRow, startCol) =
            if (diagonalIndex < nCols)
                Pair(0, diagonalIndex)
            else
                Pair(diagonalIndex - nCols + 1, 0)
        val diagonal = generateSequence(0) { it + 1 }
            .takeWhile { i -> startRow + i < nRows && startCol + i < nCols }
            .map { i -> matrix[startRow + i][startCol + i] }
            .toList()
        diagonal.ifEmpty { null }
    }
}

fun <T> rotate90(matrix: List<List<T>>): List<List<T>> = transpose(matrix).map(List<T>::reversed)

// Helper functions

tailrec fun <T> doTimes(n: Int, target: T, transform: (T) -> (T)): T =
    if (n < 1)
        target
    else
        doTimes(n - 1, transform(target), transform)

fun countAcrossFourRotations(matrix: List<List<String>>, countBy: (List<List<String>>) -> Int): Int =
    (0 until 4).sumOf { doTimes(it, matrix, ::rotate90).let(countBy) }


// Parse data

fun pathToString(path: String): String = File(path).inputStream().bufferedReader().readText()

//val fileName = "testData.txt"
val fileName = "realData.txt"

val charTable = pathToString(fileName).split("\n").map { it.split("").filter { it.isNotEmpty() } }

// count XMAS

val XMAS = "XMAS"

fun countXmasInRow(row: String): Int = row.windowed(XMAS.length).count { it == XMAS }

fun countHorizontalDiagonalXmas(matrix: List<List<String>>): Int =
    (matrix + getMainDiagonals(matrix)).sumOf { row -> countXmasInRow(row.joinToString("")) }

fun countXmas(matrix: List<List<String>>) =
    countAcrossFourRotations(matrix, ::countHorizontalDiagonalXmas)

// result

println("Count of XMAS: ${countXmas(charTable)}")

// part 2

// Count X-MAS

fun countMasCrossInRow(matrix: List<List<String>>): Int = matrix.windowed(
    3
).sumOf { window ->
    val top = window.first()
    val middle = window[1]
    val bottom = window.last()

    middle.indices.drop(1).dropLast(1).count { i ->
        top[i - 1] == "M" && top[i + 1] == "S" &&
                middle[i] == "A" &&
                bottom[i - 1] == "M" && bottom[i + 1] == "S"
    }
}

fun countMasCross(matrix: List<List<String>>): Int =
    countAcrossFourRotations(matrix, ::countMasCrossInRow)

// result

println("count of MAS crossed ${countMasCross(charTable)}")