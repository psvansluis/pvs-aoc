#!/usr/bin/env kotlin

import java.io.File

val mode = "example"

val lines = File("$mode.txt").readLines()

typealias Bank = List<Int>

val parsedLines: List<Bank> = lines.map { line -> line.chunked(1).map(String::toInt) }

fun Bank.getLargestJoltagePart1(): Int {
    val firstDigit = this.dropLast(1).max()
    val secondDigit = this.dropWhile { it != firstDigit }.drop(1).max()
    return (firstDigit * 10) + secondDigit
}

parsedLines.sumOf(Bank::getLargestJoltagePart1).let(::println)

fun <I, O> memoize(memo: MutableMap<I, O>, f: (I) -> O): (I) -> O = { input -> memo.getOrPut(input) { f(input) } }

val memo = mutableMapOf<Bank, Long>()

tailrec fun Bank.getLargestJoltagePart2(acc: Long = 0L, length: Int): Long {
    if (length == 0) return acc
    val digitToAdd = this.dropLast(length - 1).max()
    val rest = this.dropWhile { it != digitToAdd }.drop(1)
    return rest.getLargestJoltagePart2((acc * 10) + digitToAdd, length - 1)
}

parsedLines.map { line -> line.getLargestJoltagePart2(length =  12) }.sum().let(::println)