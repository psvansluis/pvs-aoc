#!/usr/bin/env kotlin

import java.io.File

val mode = "input"

val line = File("data/day2/$mode.txt").readLines()

data class IdRange(val start: Long, val end: Long)

val parsedLines = line[0].split(",").map { rangeRaw ->
    val split = rangeRaw.split("-")
    IdRange(split[0].toLong(), split[1].toLong())
}

fun isInvalidId(number: Long): Boolean {
    val str = number.toString()
    if (str.length % 2 != 0) return false
    val chunkLength = str.length / 2
    val fst = str.take(chunkLength)
    val snd = str.drop(chunkLength)
    return fst == snd
}

fun getInvalidIds(idRange: IdRange): List<Long> = (idRange.start..idRange.end).filter(::isInvalidId)

println(parsedLines.flatMap(::getInvalidIds).sum())