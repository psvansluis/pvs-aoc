#!/usr/bin/env kotlin

import java.io.File

val mode = "input"

val lines = File("$mode.txt").readLines()

data class Coordinate(val x: Int, val y: Int, val z: Int)

fun parseLine(line: String): Coordinate = line.split(",").map(String::toInt).let { Coordinate(it[0], it[1], it[2]) }

val coordinates = lines.map(::parseLine)

fun Int.`²`(): Long = this.toLong() * this

fun getDistance(a: Coordinate, b: Coordinate): Long = (a.x - b.x).`²`() + (a.y - b.y).`²`() + (a.z - b.z).`²`()

fun <T> List<T>.combinations(): List<Pair<T, T>> = if (this.size < 2) {
    emptyList()
} else {
    val head = this.first()
    val tail = this.drop(1)
    return tail.combinations() + tail.map(head::to)
}

typealias Circuit = Set<Coordinate>

fun connectCircuits(circuits: Set<Circuit>, a: Coordinate, b: Coordinate): Set<Circuit> {
    val circuitA = circuits.find { c -> c.contains(a) } ?: setOf(a)
    val circuitB = circuits.find { c -> c.contains(b) } ?: setOf(b)
    val remainder: Set<Circuit> = circuits.filterNot { c -> c.contains(a) || c.contains(b) }.toSet()
    val joinedCircuit: Circuit = circuitA.union(circuitB)
    return remainder + setOf(joinedCircuit)
}

fun productOf3LargestCircuits(coordinates: List<Coordinate>): Long =
    coordinates.combinations().asSequence() // get all combinations
        .sortedBy { (a, b) -> getDistance(a, b) }.take(1000) // get 1000 closest pairs
        .fold(emptySet<Circuit>()) { circuits, (a, b) -> connectCircuits(circuits, a, b) }.map(Circuit::size) // connect the pairs as circuits
        .sortedDescending().take(3).fold(1L) { acc, element -> acc * element } // get product of 3 largest circuits

println("part1 answer: " + productOf3LargestCircuits(coordinates))

fun getLastPairConnected(coordinates: List<Coordinate>): Pair<Coordinate, Coordinate> {
    val coordinatePairsByDistance = coordinates.combinations().sortedBy { (a, b) -> getDistance(a, b) }
    var circuits = setOf<Circuit>()
    for (coordinatePair in coordinatePairsByDistance) {
        circuits = connectCircuits(circuits, coordinatePair.first, coordinatePair.second)
        if (circuits.any { circuit -> circuit.containsAll(coordinates) }) {
            return coordinatePair
        }
    }
    throw Exception("cannot connect all circuits; should not happen")
}

println("part2 answer: " + getLastPairConnected(coordinates).let { (a, b) -> a.x.toLong() * b.x })

