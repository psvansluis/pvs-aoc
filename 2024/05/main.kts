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

    fun sort(rules: List<PageOrderingRule>): Update {
        val forbiddenFollowers: Map<Int, List<Int>> = rules
            .fold(mapOf()) { map, rule -> map + (rule.before to (map[rule.before] ?: emptyList()) + rule.after) }
        return pageNumbers.sortedWith { o1, o2 ->
            if (forbiddenFollowers[o1]?.contains(o2) == true) {
                -1
            } else {
                0
            }
        }.let(::Update)
    }
}

// parse

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
        .map { line -> Update(line.split(",").map(String::toInt)) }

// part 1
updates
    .filter { update -> pageOrderingRules.all { rule -> update.satisfies(rule) } }
    .sumOf(Update::median)


// part 2
updates
    .filterNot { update -> pageOrderingRules.all { rule -> update.satisfies(rule) } }
    .map { it.sort(pageOrderingRules) }
    .sumOf(Update::median)
