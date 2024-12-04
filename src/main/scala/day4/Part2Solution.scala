package day4

import scala.io.Source

object Part2Solution extends App:

  private val stringsToSearch = List("MAS", "SAM")

  import day4.WordSearch.Index

  private val input = Source.fromResource("day4/input.txt").getLines.toList
  private val wordSearch = WordSearch(input)

  private val result = wordSearch.getAllIndexesForCharacter('A')
    .map(computeNeighbors(wordSearch))
    .filter(_.nonEmpty)
    .count(isXMas(wordSearch))

  println(s"The result is $result")

  private def computeNeighbors(wordSearch: WordSearch)(index: Index) =
    val minRow = Math.max(index.row - 1, 0)
    val maxRow = Math.min(index.row + 1, wordSearch.height - 1)
    val minColumn = Math.max(index.column - 1, 0)
    val maxColumn = Math.min(index.column + 1, wordSearch.width - 1)

    if (index.row > 0 && index.row < wordSearch.height - 1 && index.column > 0 && index.column < wordSearch.width - 1) {
      List(
        List(Index(index.row - 1, index.column - 1), index.copy(), Index(index.row + 1, index.column + 1)),
        List(Index(index.row - 1, index.column + 1), index.copy(), Index(index.row + 1, index.column - 1))
      )
    } else {
      List.empty
    }

  private def isXMas(wordSearch: WordSearch)(indexes: List[List[Index]]) =
    val principal = stringsToSearch.contains(wordSearch(indexes.head))
    val secondary = stringsToSearch.contains(wordSearch(indexes.tail.head))

    principal && secondary
