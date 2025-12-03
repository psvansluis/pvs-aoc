#!/usr/bin/env kotlin

import java.io.File

val mode = "input"

val lines = File("$mode.txt").readLines()

typealias Bank = List<Int>

val parsedLines: List<Bank> = lines.map { line -> line.chunked(1).map(String::toInt) }

fun Bank.getLargestJoltagePart1(): Int {
    val firstDigit = this.dropLast(1).max()
    val secondDigit = this.dropWhile { it != firstDigit }.drop(1).max()
    return (firstDigit * 10) + secondDigit
}

parsedLines.sumOf(Bank::getLargestJoltagePart1).let(::println)