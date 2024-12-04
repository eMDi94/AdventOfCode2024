package day4

import scala.io.Source

object Part1Solution extends App:

  import day4.WordSearch.Index

  private val stringsToSearch = List("XMAS", "SAMX")

  private val input = Source.fromResource("day4/input.txt").getLines.toList
  private val wordSearch = WordSearch(input)

  private val result = wordSearch.getAllIndexesForCharacter('X')
    .flatMap(computeNeighbors(wordSearch) andThen wordSearch.apply)
    .count(stringsToSearch.contains)

  println(s"The result is $result")

  private def computeNeighbors(wordSearch: WordSearch)(index: Index): List[List[Index]] =
    val minRow = Math.max(index.row - 3, 0)
    val maxRow = Math.min(index.row + 3, wordSearch.height - 1)
    val minColumn = Math.max(index.column - 3, 0)
    val maxColumn = Math.min(index.column + 3, wordSearch.width - 1)

    List(
      (minRow to index.row).map(Index(_, index.column)).toList,
      (index.row to maxRow).map(Index(_, index.column)).toList,
      (minColumn to index.column).map(Index(index.row, _)).toList,
      (index.column to maxColumn).map(Index(index.row, _)).toList,
      (0 to Math.min(index.row - minRow, index.column - minColumn)).map(i => Index(index.row - i, index.column - i)).toList,
      (0 to Math.min(maxRow - index.row, index.column - minColumn)).map(i => Index(index.row + i, index.column - i)).toList,
      (0 to Math.min(index.row - minRow, maxColumn - index.column)).map(i => Index(index.row - i, index.column + i)).toList,
      (0 to Math.min(maxRow - index.row, maxColumn - index.column)).map(i => Index(index.row + i, index.column + i)).toList
    )
