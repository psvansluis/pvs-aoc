import java.io.File

fun pathToString(path: String): String = File(path).inputStream().bufferedReader().readText()

//val fileName = "testData.txt"
val fileName = "realData.txt"

val lines = pathToString(fileName).split("\n")

class PageOrderingRule(val before: Int, val after: Int)

class Update(val pageNumbers: List<Int>) {
    val median = pageNumbers[pageNumbers.size / 2]

    fun satisfies(rule: PageOrderingRule): Boolean {
        val beforeIndex = pageNumbers.indexOf(rule.before)
        val afterIndex = pageNumbers.indexOf(rule.after)
        return beforeIndex < 0 || afterIndex < 0 || beforeIndex < afterIndex
    }
}

val pageOrderingRules: List<PageOrderingRule> =
    lines
        .takeWhile(String::isNotEmpty)
        .map { line ->
            val numbers = line.split('|').map(String::toInt)
            PageOrderingRule(numbers[0], numbers[1])
        }

val updates: List<Update> =
    lines
        .dropWhile(String::isNotEmpty)
        .drop(1)
        .map { line ->
            Update(line.split(",").map(String::toInt))
        }

updates
    .filter { update -> pageOrderingRules.all { rule -> update.satisfies(rule) } }
    .sumOf { it.median }