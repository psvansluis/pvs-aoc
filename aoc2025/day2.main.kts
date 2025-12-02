#!/usr/bin/env kotlin

import java.io.File

val mode = "input"

val line = File("data/day2/$mode.txt").readLines()

data class IdRange(val start: Long, val end: Long)

val parsedLines = line[0].split(",").map { rangeRaw ->
    val split = rangeRaw.split("-")
    IdRange(split[0].toLong(), split[1].toLong())
}

fun isInvalidIdPart1(number: Long): Boolean {
    val str = number.toString()
    if (str.length % 2 != 0) return false
    val chunkLength = str.length / 2
    val fst = str.take(chunkLength)
    val snd = str.drop(chunkLength)
    return fst == snd
}

fun getInvalidIds(idRange: IdRange, validationMethod: (Long) -> Boolean): List<Long> =
    (idRange.start..idRange.end).filter(validationMethod)

fun getInvalidIdsPart1(idRange: IdRange): List<Long> = getInvalidIds(idRange, ::isInvalidIdPart1)

println(parsedLines.flatMap(::getInvalidIdsPart1).sum())

tailrec fun isInvalidIdPart2(number: Long, nPartitions: Int = 2): Boolean {
    val str = number.toString()
    if (nPartitions > str.length) return false
    val chunkLength = str.length / nPartitions
    val substrings = str.chunked(chunkLength)
    return substrings.all { it == substrings[0] } || isInvalidIdPart2(number, nPartitions + 1)
}

fun getInvalidIdsPart2(idRange: IdRange): List<Long> = getInvalidIds(idRange, ::isInvalidIdPart2)

println(parsedLines.flatMap(::getInvalidIdsPart2).sum())