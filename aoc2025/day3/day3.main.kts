#!/usr/bin/env kotlin

import java.io.File

val mode = "input"

val lines = File("$mode.txt").readLines()

typealias Bank = List<Int>

val parsedLines: List<Bank> = lines.map { line -> line.chunked(1).map(String::toInt) }

tailrec fun Bank.getLargestJoltage(acc: Long = 0L, length: Int): Long {
    if (length == 0) return acc
    val digitToAdd = this.dropLast(length - 1).max()
    val rest = this.dropWhile { it != digitToAdd }.drop(1)
    return rest.getLargestJoltage((acc * 10) + digitToAdd, length - 1)
}

print("part 1: ")
parsedLines.sumOf { bank -> bank.getLargestJoltage(length = 2) }.let(::println)

print("part 2: ")
parsedLines.sumOf { bank -> bank.getLargestJoltage(length = 12) }.let(::println)