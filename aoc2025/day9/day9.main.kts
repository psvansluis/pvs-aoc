#!/usr/bin/env kotlin

import java.io.File
import kotlin.math.abs

val mode = "input"

val lines = File("$mode.txt").readLines()

data class Tile(val x: Int, val y: Int)

val tiles = lines.map { it.split(",").map(String::toInt) }.map { Tile(it[0], it[1]) }

fun <T> List<T>.combinations(): List<Pair<T, T>> = if (this.size < 2) {
    emptyList()
} else {
    val head = this.first()
    val tail = this.drop(1)
    return tail.combinations() + tail.map(head::to)
}

fun squareSize(tiles: Pair<Tile, Tile>): Long {
    val xDiff = abs(tiles.first.x - tiles.second.x) + 1
    val yDiff = abs(tiles.first.y - tiles.second.y) + 1
    return (xDiff.toLong() * yDiff).also { println("${tiles.first} by ${tiles.second} has an area of $it") }
}

tiles.combinations().maxOfOrNull(::squareSize).let(::println)