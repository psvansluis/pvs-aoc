import java.io.File


//val fileName = "testData.txt"
val fileName = "realData.txt"

fun pathToString(path: String): String = File(path).inputStream().bufferedReader().readText()

