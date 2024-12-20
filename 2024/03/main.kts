import java.io.File

val fileName = "realData.txt"
//val fileName = "testData.txt"
val inputStream = File(fileName).inputStream()
val inputString = inputStream.bufferedReader().readText()

val regex = Regex("mul\\((\\d+),(\\d+)\\)")
val muls: List<List<Int>> =
    regex.findAll(inputString).toList().map { match -> match.groups.drop(1).map { it!!.value.toInt() } }

val sumOfProducts = muls.sumOf { it[0] * it[1] }
println("sumOfProducts: $sumOfProducts")