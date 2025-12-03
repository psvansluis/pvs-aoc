#!/usr/bin/env kotlin
import java.io.File

data class Rotation(
    val distance: Int,
    val direction: Direction,
)

enum class Direction {
    LEFT, RIGHT
}

val mode = "input"

val lines = File("$mode.txt").readLines()

val parsedLines = lines.map { line ->
    Rotation(
        direction = if (line[0] == 'L') Direction.LEFT else Direction.RIGHT, distance = line.drop(1).toInt()
    )
}

data class Dial(val position: Int = 50, val timesAtZero: Int = 0)

fun rotate(dial: Dial, rotation: Rotation): Dial {
    val dist = when (rotation.direction) {
        Direction.LEFT -> -1
        Direction.RIGHT -> 1
    } * rotation.distance

    val newPos = (dial.position + dist + 100) % 100
    val newTimesAtZero = dial.timesAtZero + if (newPos == 0) 1 else 0

    return Dial(newPos, newTimesAtZero)
}

val dialAtEnd = parsedLines.fold(Dial(), ::rotate)
println(dialAtEnd)

val dialAtEndPart2 = parsedLines.flatMap { rotation ->
    generateSequence { Rotation(direction = rotation.direction, distance = 1) }.take(rotation.distance)
}.fold(Dial(), ::rotate)
println(dialAtEndPart2)