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

val compacted = disk.compact()
val checksum: Long = compacted.foldIndexed(0L) { index: Int, acc: Long, block: Int ->
    acc + (block * index)
}

println(checksum)