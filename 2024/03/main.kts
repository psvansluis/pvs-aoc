import java.io.File

val fileName = "realData.txt"
//val fileName = "testData2.txt"
val inputStream = File(fileName).inputStream()
val inputString = inputStream.bufferedReader().readText()

val regexMul = Regex("mul\\((\\d+),(\\d+)\\)")

fun getMuls(input: String): Sequence<List<Int>> =
    regexMul.findAll(input).map { match -> match.groups.drop(1).map { it!!.value.toInt() } }

val muls: Sequence<List<Int>> = getMuls(inputString)

val sumOfProducts = muls.sumOf { it[0] * it[1] }
println("sumOfProducts: $sumOfProducts")

// part 2
val doParts: String =
    ("do()$inputString")
        .split("don't()")
        .flatMap { it.split("do()").drop(1) }
        .joinToString("")

val sumOfEnabledProducts = getMuls(doParts).sumOf { it[0] * it[1] }
println("sumOfEnabledProducts: $sumOfEnabledProducts")
