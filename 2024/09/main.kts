import java.io.File

//val fileName = "testData.txt"
val fileName = "realData.txt"

fun pathToString(path: String): String = File(path).inputStream().bufferedReader().readText()

val disk: List<Int?> = pathToString(fileName)
    .flatMapIndexed { i, ch -> List(ch.digitToInt()) { if (i % 2 == 0) i / 2 else null } }
    .dropLastWhile { it == null }

fun <T> Iterable<T>.span(p: (T) -> Boolean): Pair<List<T>, List<T>> = takeWhile(p) to dropWhile(p)

tailrec fun <T> T.doUntil(f: (T) -> T, predicate: (T) -> Boolean): T =
    when {
        predicate(this) -> this
        else -> f(this).doUntil(f, predicate)
    }

fun List<Int?>.compactOnce(): List<Int?> {
    val (pre, mid) = dropLastWhile { it == null }.span { it != null }
    val blockToMove = mid.last()
    val post = mid.drop(1).dropLast(1)
    return pre + blockToMove + post
}

fun List<Int?>.compact(): List<Int> =
    doUntil(f = { it.compactOnce() }, predicate = { it.none { block -> block == null } }) as List<Int>

fun List<Int?>.checksum(): Long = foldIndexed(0L) { index: Int, acc: Long, block: Int? ->
    acc + ((block ?: 0) * index)
}

val compacted = disk.compact()
val checksumPart1: Long = compacted.checksum()

println(checksumPart1)

// part 2
fun List<Int?>.compactFiles(): List<Int?> {
    val lastFile = this.findLast { it != null }!!
    tailrec fun compactFile(disk: List<Int?>, fileIndex: Int): List<Int?> {
        if (fileIndex < 1) return disk
        val file = disk.dropWhile { it != fileIndex }.takeWhile { it == fileIndex }
        val indexOfOpenSpot =
            disk.takeWhile { it != fileIndex }.windowed(file.size).indexOfFirst { it.all { it == null } }
        val nextIndex = fileIndex - 1
        if (indexOfOpenSpot == -1) {
            return compactFile(disk, nextIndex)
        } else {
            val pre = disk.take(indexOfOpenSpot)
            val post = disk.drop(indexOfOpenSpot).drop(file.size).map { if (it == fileIndex) null else it }
            return compactFile(pre + file + post, nextIndex)
        }
    }
    return compactFile(this, lastFile)
}

val checksumPart2 = disk.compactFiles().checksum()
println(checksumPart2)
